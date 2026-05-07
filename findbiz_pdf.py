"""findbiz_pdf.py

輸入統一編號 (統編)，自動到 https://findbiz.nat.gov.tw 查詢，
點開查詢結果第一筆公司，將每個頁籤分別存成 PDF。

用法：
    python3 findbiz_pdf.py 04595257
    python3 findbiz_pdf.py 04595257 -o ./downloads
    python3 findbiz_pdf.py 04595257 --headed   # 顯示瀏覽器 (除錯用)

需求：
    pip install playwright
    python -m playwright install chromium
"""
from __future__ import annotations

import argparse
import re
import sys
import time
from pathlib import Path

from playwright.sync_api import (
    Page,
    Frame,
    Locator,
    TimeoutError as PWTimeout,
    sync_playwright,
)

QUERY_URL = "https://findbiz.nat.gov.tw/fts/query/QueryBar/queryInit.do"

# 偵測詳細頁面內常見的頁籤 (label 文字)；找不到時退回掃描所有 tablist 連結。
KNOWN_TAB_LABELS = [
    "基本資料",
    "董監事資料",
    "經理人資料",
    "分公司資料",
    "工廠資料",
    "商業登記",
    "有限合夥",
    "證照資料",
    "歷史資料",
]


def slugify(name: str) -> str:
    name = (name or "").strip()
    name = re.sub(r"[\\/:*?\"<>|\s]+", "_", name)
    return name or "tab"


def find_main_frame(page: Page) -> Page | Frame:
    """findbiz 部分頁面把內容放在 iframe；自動找出有搜尋欄的 frame。"""
    for f in [page, *page.frames]:
        try:
            if f.locator('input[name="qryCond"]').count() > 0:
                return f
        except Exception:
            continue
    return page


def fill_tax_id_and_search(page: Page, tax_id: str) -> Page | Frame:
    page.goto(QUERY_URL, wait_until="domcontentloaded")
    page.wait_for_load_state("networkidle")

    frame = find_main_frame(page)

    # 切換為「統一編號」模式 (頁面上是 radio 或下拉)。找不到也沒關係，
    # qryCond 對純數字輸入會自動命中統編。
    for sel in [
        'input[type="radio"][value="bID"]',
        'input[type="radio"][value*="ID"]',
        'label:has-text("統一編號")',
    ]:
        try:
            loc = frame.locator(sel).first
            if loc.count() > 0:
                loc.click(timeout=1500)
                break
        except Exception:
            pass

    keyword = frame.locator('input[name="qryCond"]').first
    keyword.wait_for(state="visible", timeout=15000)
    keyword.fill(tax_id)

    # 查詢按鈕
    clicked = False
    for sel in [
        'input[type="submit"][value*="查詢"]',
        'button:has-text("查詢")',
        'a:has-text("查詢")',
        'input[value="查詢"]',
    ]:
        try:
            btn = frame.locator(sel).first
            if btn.count() > 0:
                btn.click()
                clicked = True
                break
        except Exception:
            continue
    if not clicked:
        keyword.press("Enter")

    page.wait_for_load_state("networkidle")
    return find_main_frame(page)


def open_first_result(page: Page, frame: Page | Frame, tax_id: str) -> Page:
    """點開查詢結果第一筆。findbiz 通常會開新分頁。"""
    context = page.context

    # 第一筆結果連結。優先找含統編字串的 a，再退回結果區塊內第一個 a。
    candidates = [
        f'a:has-text("{tax_id}")',
        'table a[href*="qryCmpyDetail"]',
        'a[href*="qryCmpyDetail"]',
        'table.table a',
        '.resultList a',
    ]
    link: Locator | None = None
    for sel in candidates:
        try:
            loc = frame.locator(sel).first
            if loc.count() > 0:
                link = loc
                break
        except Exception:
            continue
    if link is None:
        raise RuntimeError("找不到查詢結果，請確認統編是否正確")

    # 點擊；若會開新分頁則接住
    try:
        with context.expect_page(timeout=4000) as pinfo:
            link.click()
        new_page = pinfo.value
        new_page.wait_for_load_state("networkidle")
        return new_page
    except PWTimeout:
        page.wait_for_load_state("networkidle")
        return page


def discover_tabs(frame: Page | Frame) -> list[Locator]:
    """回傳詳細頁面上的頁籤 Locator 列表。"""
    selectors = [
        '[role="tab"]',
        'ul.nav-tabs a',
        'ul.nav a',
        '.tab a',
        '.tabs a',
    ]
    for sel in selectors:
        loc = frame.locator(sel)
        if loc.count() >= 2:
            return [loc.nth(i) for i in range(loc.count())]

    # 退而求其次：透過已知中文標籤搜尋
    found: list[Locator] = []
    for label in KNOWN_TAB_LABELS:
        loc = frame.get_by_role("link", name=label)
        if loc.count() == 0:
            loc = frame.locator(f'a:has-text("{label}")')
        if loc.count() > 0:
            found.append(loc.first)
    return found


def get_company_name(frame: Page | Frame, fallback: str) -> str:
    for sel in [
        'h1', 'h2', '.cmpyName', '.title',
        'td:has-text("公司名稱") + td',
        'th:has-text("公司名稱") + td',
    ]:
        try:
            loc = frame.locator(sel).first
            if loc.count() > 0:
                txt = loc.inner_text(timeout=1500).strip()
                if txt and len(txt) < 80:
                    return txt
        except Exception:
            continue
    return fallback


def save_pdf(page: Page, out_path: Path) -> None:
    out_path.parent.mkdir(parents=True, exist_ok=True)
    # 列印背景色、A4
    page.emulate_media(media="screen")
    page.pdf(
        path=str(out_path),
        format="A4",
        print_background=True,
        margin={"top": "10mm", "bottom": "10mm", "left": "8mm", "right": "8mm"},
    )


def download_all_tabs(page: Page, out_dir: Path, prefix: str) -> int:
    frame = find_main_frame(page)
    tabs = discover_tabs(frame)

    if not tabs:
        # 找不到頁籤 → 整頁存一張
        save_pdf(page, out_dir / f"{prefix}.pdf")
        print(f"[saved] {prefix}.pdf  (沒偵測到頁籤，整頁輸出)")
        return 1

    saved = 0
    seen: set[str] = set()
    for i, tab in enumerate(tabs):
        try:
            label = tab.inner_text(timeout=2000).strip()
        except Exception:
            label = f"tab_{i}"
        if not label or label in seen:
            continue
        seen.add(label)

        try:
            tab.scroll_into_view_if_needed(timeout=2000)
            tab.click(timeout=5000)
        except Exception as e:
            print(f"[skip] 點不到頁籤「{label}」: {e}")
            continue

        try:
            page.wait_for_load_state("networkidle", timeout=15000)
        except PWTimeout:
            pass
        time.sleep(0.6)  # 等 JS 動畫/AJAX 完成

        out_file = out_dir / f"{prefix}_{saved + 1:02d}_{slugify(label)}.pdf"
        try:
            save_pdf(page, out_file)
            print(f"[saved] {out_file.name}")
            saved += 1
        except Exception as e:
            print(f"[fail ] {label}: {e}")
    return saved


def main() -> int:
    ap = argparse.ArgumentParser(description="findbiz 自動查詢並下載各頁籤 PDF")
    ap.add_argument("tax_id", help="統一編號，例如 04595257")
    ap.add_argument("-o", "--out", default="output", help="PDF 輸出根目錄 (預設 ./output)")
    ap.add_argument("--headed", action="store_true", help="顯示瀏覽器視窗 (預設 headless)")
    ap.add_argument("--timeout", type=int, default=30000, help="預設操作 timeout (ms)")
    args = ap.parse_args()

    if not re.fullmatch(r"\d{8}", args.tax_id):
        print("警告：統編應為 8 位數字", file=sys.stderr)

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=not args.headed)
        context = browser.new_context(locale="zh-TW", viewport={"width": 1366, "height": 900})
        context.set_default_timeout(args.timeout)
        page = context.new_page()

        try:
            print(f"→ 開啟查詢頁面，搜尋統編 {args.tax_id}")
            frame = fill_tax_id_and_search(page, args.tax_id)

            print("→ 點開第一筆查詢結果")
            detail = open_first_result(page, frame, args.tax_id)

            company = get_company_name(find_main_frame(detail), args.tax_id)
            print(f"→ 公司：{company}")

            out_dir = Path(args.out) / f"{args.tax_id}_{slugify(company)}"
            count = download_all_tabs(detail, out_dir, args.tax_id)
            print(f"\n完成，共輸出 {count} 個 PDF 至 {out_dir}")
            return 0
        except Exception as e:
            print(f"\n發生錯誤：{e}", file=sys.stderr)
            return 1
        finally:
            context.close()
            browser.close()


if __name__ == "__main__":
    sys.exit(main())
