"""findbiz_pdf.py

輸入統一編號 (統編)，自動到 https://findbiz.nat.gov.tw 查詢，
點開查詢結果第一筆公司，將每個頁籤分別存成 PDF。

用法：
    # 第一次 (有頭模式，必要時手動通過 Cloudflare 驗證；通過後自動存 session)
    python3 findbiz_pdf.py 04595257 --headed

    # 之後都可以無頭直接跑 (帶上次通過驗證後的 cookies)
    python3 findbiz_pdf.py 04595257

需求：
    pip install playwright
    python -m playwright install chromium
    # 建議另外安裝本機 Chrome：python -m playwright install chrome
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
    BrowserContext,
    TimeoutError as PWTimeout,
    sync_playwright,
)

QUERY_URL = "https://findbiz.nat.gov.tw/fts/query/QueryBar/queryInit.do"

DEFAULT_UA = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
    "AppleWebKit/537.36 (KHTML, like Gecko) "
    "Chrome/131.0.0.0 Safari/537.36"
)

# 注入到每個頁面，掩蓋常見的自動化指紋 (供 Cloudflare 等檢測通過)
STEALTH_JS = """
Object.defineProperty(navigator, 'webdriver', { get: () => undefined });
Object.defineProperty(navigator, 'languages', { get: () => ['zh-TW', 'zh', 'en-US', 'en'] });
Object.defineProperty(navigator, 'plugins',   { get: () => [1, 2, 3, 4, 5] });
window.chrome = window.chrome || { runtime: {} };
const originalQuery = window.navigator.permissions && window.navigator.permissions.query;
if (originalQuery) {
  window.navigator.permissions.query = (parameters) =>
    parameters.name === 'notifications'
      ? Promise.resolve({ state: Notification.permission })
      : originalQuery(parameters);
}
"""

# 偵測詳細頁面常見的頁籤 (label 文字)；找不到時退回掃描所有 tablist 連結。
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

CLOUDFLARE_HINTS = [
    "正在執行安全驗證",
    "Just a moment",
    "Checking your browser",
    "challenge-platform",
    "cf-browser-verification",
    "Cloudflare",
]


def slugify(name: str) -> str:
    name = (name or "").strip()
    name = re.sub(r"[\\/:*?\"<>|\s]+", "_", name)
    return name or "tab"


# ---------------------------------------------------------------------------
# Cloudflare handling
# ---------------------------------------------------------------------------
def looks_like_cloudflare(page: Page) -> bool:
    try:
        title = (page.title() or "").lower()
    except Exception:
        title = ""
    if any(h.lower() in title for h in ("just a moment", "請稍候", "attention required")):
        return True
    try:
        body_text = page.locator("body").inner_text(timeout=1500)
    except Exception:
        return False
    return any(h in body_text for h in CLOUDFLARE_HINTS)


def wait_through_cloudflare(page: Page, headed: bool, timeout_s: int = 90) -> None:
    """若停在 Cloudflare 驗證頁，就等驗證通過 (有頭模式時提示使用者操作)。"""
    if not looks_like_cloudflare(page):
        return

    print("⚠ 偵測到 Cloudflare 驗證頁。", file=sys.stderr)
    if headed:
        print(
            "  請在瀏覽器視窗手動完成驗證 (有時是點選方框、有時自動通過)，\n"
            "  通過後本程式會自動繼續。最多等 90 秒…",
            file=sys.stderr,
        )
    else:
        print(
            "  目前是 headless 模式，極可能無法自動通過。\n"
            "  建議：先用 --headed 跑一次手動通過驗證，session 會被存起來，\n"
            "  之後再用 headless 跑。",
            file=sys.stderr,
        )

    deadline = time.time() + timeout_s
    while time.time() < deadline:
        time.sleep(1.5)
        if not looks_like_cloudflare(page):
            print("✓ 已通過 Cloudflare 驗證", file=sys.stderr)
            return
    raise RuntimeError("等待 Cloudflare 驗證逾時，請改用 --headed 手動通過後再試")


# ---------------------------------------------------------------------------
# Page interaction
# ---------------------------------------------------------------------------
def find_main_frame(page: Page) -> Page | Frame:
    """findbiz 部分頁面把內容放在 iframe；自動找出有搜尋欄的 frame。"""
    for f in [page, *page.frames]:
        try:
            if f.locator('input[name="qryCond"]').count() > 0:
                return f
        except Exception:
            continue
    return page


def fill_tax_id_and_search(page: Page, tax_id: str, headed: bool) -> Page | Frame:
    page.goto(QUERY_URL, wait_until="domcontentloaded")
    wait_through_cloudflare(page, headed=headed)
    page.wait_for_load_state("networkidle")

    frame = find_main_frame(page)

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
    wait_through_cloudflare(page, headed=headed)
    return find_main_frame(page)


def open_first_result(page: Page, frame: Page | Frame, tax_id: str, headed: bool) -> Page:
    """點開查詢結果第一筆。findbiz 通常會開新分頁。"""
    context = page.context

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

    try:
        with context.expect_page(timeout=4000) as pinfo:
            link.click()
        new_page = pinfo.value
        new_page.wait_for_load_state("networkidle")
        wait_through_cloudflare(new_page, headed=headed)
        return new_page
    except PWTimeout:
        page.wait_for_load_state("networkidle")
        wait_through_cloudflare(page, headed=headed)
        return page


def discover_tabs(frame: Page | Frame) -> list[Locator]:
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
        time.sleep(0.6)

        out_file = out_dir / f"{prefix}_{saved + 1:02d}_{slugify(label)}.pdf"
        try:
            save_pdf(page, out_file)
            print(f"[saved] {out_file.name}")
            saved += 1
        except Exception as e:
            print(f"[fail ] {label}: {e}")
    return saved


# ---------------------------------------------------------------------------
# Browser bootstrap (with anti-detection)
# ---------------------------------------------------------------------------
def build_context(p, args) -> tuple[BrowserContext, object]:
    """嘗試用本機 Chrome 啟動；失敗就退回打包好的 chromium。"""
    launch_kwargs = dict(
        headless=not args.headed,
        args=[
            "--disable-blink-features=AutomationControlled",
            "--disable-features=IsolateOrigins,site-per-process",
            "--no-sandbox",
        ],
        ignore_default_args=["--enable-automation"],
    )

    browser = None
    last_err: Exception | None = None
    channels_to_try = [args.channel] if args.channel else ["chrome", "msedge", None]
    for ch in channels_to_try:
        try:
            kw = dict(launch_kwargs)
            if ch:
                kw["channel"] = ch
            browser = p.chromium.launch(**kw)
            if ch:
                print(f"→ 使用本機瀏覽器 channel={ch}")
            else:
                print("→ 使用打包的 Chromium (建議 `playwright install chrome` 改用本機 Chrome)")
            break
        except Exception as e:
            last_err = e
            continue
    if browser is None:
        raise RuntimeError(f"無法啟動瀏覽器：{last_err}")

    state_path = Path(args.state)
    storage_state = str(state_path) if state_path.exists() else None
    if storage_state:
        print(f"→ 載入既有 session：{state_path}")

    context = browser.new_context(
        user_agent=args.user_agent,
        locale="zh-TW",
        timezone_id="Asia/Taipei",
        viewport={"width": 1366, "height": 900},
        storage_state=storage_state,
    )
    context.set_default_timeout(args.timeout)
    context.add_init_script(STEALTH_JS)
    return context, browser


def main() -> int:
    ap = argparse.ArgumentParser(description="findbiz 自動查詢並下載各頁籤 PDF")
    ap.add_argument("tax_id", help="統一編號，例如 04595257")
    ap.add_argument("-o", "--out", default="output", help="PDF 輸出根目錄 (預設 ./output)")
    ap.add_argument("--headed", action="store_true", help="顯示瀏覽器 (建議第一次使用)")
    ap.add_argument("--timeout", type=int, default=30000, help="預設操作 timeout (ms)")
    ap.add_argument("--state", default="findbiz_state.json",
                    help="存放 cookies/session 的 JSON 檔；通過 Cloudflare 後會自動寫入")
    ap.add_argument("--channel", default=None,
                    help="瀏覽器 channel：chrome / msedge / chromium (預設自動嘗試)")
    ap.add_argument("--user-agent", default=DEFAULT_UA, help="自訂 User-Agent")
    args = ap.parse_args()

    if not re.fullmatch(r"\d{8}", args.tax_id):
        print("警告：統編應為 8 位數字", file=sys.stderr)

    with sync_playwright() as p:
        context, browser = build_context(p, args)
        page = context.new_page()

        try:
            print(f"→ 開啟查詢頁面，搜尋統編 {args.tax_id}")
            frame = fill_tax_id_and_search(page, args.tax_id, headed=args.headed)

            print("→ 點開第一筆查詢結果")
            detail = open_first_result(page, frame, args.tax_id, headed=args.headed)

            company = get_company_name(find_main_frame(detail), args.tax_id)
            print(f"→ 公司：{company}")

            out_dir = Path(args.out) / f"{args.tax_id}_{slugify(company)}"
            count = download_all_tabs(detail, out_dir, args.tax_id)
            print(f"\n完成，共輸出 {count} 個 PDF 至 {out_dir}")

            # 通過驗證後保存 session，下次免驗證
            try:
                context.storage_state(path=args.state)
                print(f"→ 已更新 session：{args.state}")
            except Exception as e:
                print(f"(無法寫入 session: {e})", file=sys.stderr)

            return 0
        except Exception as e:
            print(f"\n發生錯誤：{e}", file=sys.stderr)
            return 1
        finally:
            context.close()
            browser.close()


if __name__ == "__main__":
    sys.exit(main())
