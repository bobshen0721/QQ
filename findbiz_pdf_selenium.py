"""findbiz_pdf_selenium.py

Selenium 版：輸入統一編號 (統編)，自動到 https://findbiz.nat.gov.tw 查詢，
點開查詢結果第一筆公司，將每個頁籤分別存成 PDF。

用法：
    # 第一次 (有頭模式，必要時手動通過 Cloudflare 驗證；通過後自動存 cookies)
    python3 findbiz_pdf_selenium.py 04595257 --headed

    # 之後正常跑 (帶上次的 cookies，通常免再驗證)
    python3 findbiz_pdf_selenium.py 04595257

需求：
    pip install selenium undetected-chromedriver
    # 機器需已安裝 Google Chrome；driver 會自動下載對應版本
"""
from __future__ import annotations

import argparse
import base64
import json
import re
import sys
import time
from pathlib import Path

# 優先用 undetected_chromedriver；裝不起來就退回原生 selenium
try:
    import undetected_chromedriver as uc
    HAS_UC = True
except ImportError:
    HAS_UC = False

from selenium import webdriver
from selenium.webdriver.chrome.options import Options as ChromeOptions
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import (
    NoSuchElementException,
    TimeoutException,
    WebDriverException,
)

QUERY_URL = "https://findbiz.nat.gov.tw/fts/query/QueryBar/queryInit.do"

DEFAULT_UA = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
    "AppleWebKit/537.36 (KHTML, like Gecko) "
    "Chrome/131.0.0.0 Safari/537.36"
)

STEALTH_JS = """
Object.defineProperty(navigator, 'webdriver', { get: () => undefined });
Object.defineProperty(navigator, 'languages', { get: () => ['zh-TW', 'zh', 'en-US', 'en'] });
Object.defineProperty(navigator, 'plugins',   { get: () => [1, 2, 3, 4, 5] });
window.chrome = window.chrome || { runtime: {} };
"""

KNOWN_TAB_LABELS = [
    "基本資料", "董監事資料", "經理人資料", "分公司資料", "工廠資料",
    "商業登記", "有限合夥", "證照資料", "歷史資料",
]

CLOUDFLARE_HINTS = [
    "正在執行安全驗證", "Just a moment", "Checking your browser",
    "challenge-platform", "cf-browser-verification", "Cloudflare",
]


def slugify(name: str) -> str:
    name = (name or "").strip()
    name = re.sub(r"[\\/:*?\"<>|\s]+", "_", name)
    return name or "tab"


# ---------------------------------------------------------------------------
# Driver bootstrap
# ---------------------------------------------------------------------------
def build_driver(args) -> webdriver.Chrome:
    if HAS_UC:
        print("→ 使用 undetected-chromedriver (建議；對 Cloudflare 友善)")
        opts = uc.ChromeOptions()
        opts.add_argument(f"--user-agent={args.user_agent}")
        opts.add_argument("--lang=zh-TW")
        opts.add_argument("--window-size=1366,900")
        if not args.headed:
            opts.add_argument("--headless=new")
        # uc 自己會處理 enable-automation flag 與 navigator.webdriver
        driver = uc.Chrome(options=opts, use_subprocess=True)
    else:
        print("→ undetected-chromedriver 未安裝，退回原生 Selenium "
              "(較容易被 Cloudflare 擋；建議 `pip install undetected-chromedriver`)")
        opts = ChromeOptions()
        opts.add_argument(f"--user-agent={args.user_agent}")
        opts.add_argument("--lang=zh-TW")
        opts.add_argument("--window-size=1366,900")
        opts.add_argument("--disable-blink-features=AutomationControlled")
        opts.add_experimental_option("excludeSwitches", ["enable-automation"])
        opts.add_experimental_option("useAutomationExtension", False)
        if not args.headed:
            opts.add_argument("--headless=new")
        driver = webdriver.Chrome(options=opts)
        driver.execute_cdp_cmd(
            "Page.addScriptToEvaluateOnNewDocument", {"source": STEALTH_JS}
        )

    driver.set_page_load_timeout(args.timeout / 1000)
    return driver


def load_cookies(driver: webdriver.Chrome, state_path: Path) -> bool:
    if not state_path.exists():
        return False
    try:
        data = json.loads(state_path.read_text(encoding="utf-8"))
        # 必須先進到該網域才能 add_cookie
        driver.get("https://findbiz.nat.gov.tw/")
        for c in data:
            c.pop("sameSite", None)  # Selenium 對某些值挑剔
            try:
                driver.add_cookie(c)
            except Exception:
                pass
        print(f"→ 已載入 cookies：{state_path}")
        return True
    except Exception as e:
        print(f"(cookies 載入失敗，忽略：{e})", file=sys.stderr)
        return False


def save_cookies(driver: webdriver.Chrome, state_path: Path) -> None:
    try:
        state_path.write_text(
            json.dumps(driver.get_cookies(), ensure_ascii=False, indent=2),
            encoding="utf-8",
        )
        print(f"→ 已更新 cookies：{state_path}")
    except Exception as e:
        print(f"(cookies 寫入失敗：{e})", file=sys.stderr)


# ---------------------------------------------------------------------------
# Cloudflare
# ---------------------------------------------------------------------------
def looks_like_cloudflare(driver: webdriver.Chrome) -> bool:
    try:
        title = (driver.title or "").lower()
    except Exception:
        title = ""
    if any(h.lower() in title for h in ("just a moment", "請稍候", "attention required")):
        return True
    try:
        body_text = driver.find_element(By.TAG_NAME, "body").text
    except Exception:
        return False
    return any(h in body_text for h in CLOUDFLARE_HINTS)


def wait_through_cloudflare(driver: webdriver.Chrome, headed: bool, timeout_s: int = 90) -> None:
    if not looks_like_cloudflare(driver):
        return
    print("⚠ 偵測到 Cloudflare 驗證頁。", file=sys.stderr)
    if headed:
        print("  請於瀏覽器手動完成驗證 (有時是點選方框、有時自動通過)，最多等 90 秒…",
              file=sys.stderr)
    else:
        print("  目前是 headless，極可能無法自動通過。\n"
              "  建議：先用 --headed 跑一次，cookies 會被存起來。", file=sys.stderr)

    deadline = time.time() + timeout_s
    while time.time() < deadline:
        time.sleep(1.5)
        if not looks_like_cloudflare(driver):
            print("✓ 已通過 Cloudflare 驗證", file=sys.stderr)
            return
    raise RuntimeError("等待 Cloudflare 驗證逾時，請改用 --headed 手動通過後再試")


# ---------------------------------------------------------------------------
# Frame helpers
# ---------------------------------------------------------------------------
def switch_to_query_frame(driver: webdriver.Chrome) -> bool:
    """切到含 qryCond 搜尋框的 frame；找不到就保持在 default content。"""
    driver.switch_to.default_content()
    try:
        driver.find_element(By.NAME, "qryCond")
        return True
    except NoSuchElementException:
        pass
    for frame in driver.find_elements(By.TAG_NAME, "iframe"):
        try:
            driver.switch_to.frame(frame)
            try:
                driver.find_element(By.NAME, "qryCond")
                return True
            except NoSuchElementException:
                driver.switch_to.default_content()
        except Exception:
            driver.switch_to.default_content()
    driver.switch_to.default_content()
    return False


def find_first(driver, selectors: list[tuple[str, str]]):
    for by, sel in selectors:
        try:
            el = driver.find_element(by, sel)
            if el:
                return el
        except NoSuchElementException:
            continue
    return None


# ---------------------------------------------------------------------------
# Search & navigate
# ---------------------------------------------------------------------------
def fill_tax_id_and_search(driver: webdriver.Chrome, tax_id: str, headed: bool) -> None:
    driver.get(QUERY_URL)
    wait_through_cloudflare(driver, headed=headed)

    switch_to_query_frame(driver)

    # 切換為「統一編號」 (找不到也沒關係)
    for by, sel in [
        (By.CSS_SELECTOR, 'input[type="radio"][value="bID"]'),
        (By.CSS_SELECTOR, 'input[type="radio"][value*="ID"]'),
        (By.XPATH, "//label[contains(., '統一編號')]"),
    ]:
        try:
            driver.find_element(by, sel).click()
            break
        except Exception:
            continue

    keyword = WebDriverWait(driver, 15).until(
        EC.visibility_of_element_located((By.NAME, "qryCond"))
    )
    keyword.clear()
    keyword.send_keys(tax_id)

    btn = find_first(driver, [
        (By.CSS_SELECTOR, 'input[type="submit"][value*="查詢"]'),
        (By.XPATH, "//button[contains(., '查詢')]"),
        (By.XPATH, "//a[contains(., '查詢')]"),
        (By.CSS_SELECTOR, 'input[value="查詢"]'),
    ])
    if btn:
        btn.click()
    else:
        keyword.send_keys(Keys.ENTER)

    time.sleep(1.5)
    wait_through_cloudflare(driver, headed=headed)


def open_first_result(driver: webdriver.Chrome, tax_id: str, headed: bool) -> None:
    """點第一筆結果。findbiz 通常會開新分頁；自動切過去。"""
    switch_to_query_frame(driver)

    candidates = [
        (By.XPATH, f"//a[contains(., '{tax_id}')]"),
        (By.CSS_SELECTOR, 'table a[href*="qryCmpyDetail"]'),
        (By.CSS_SELECTOR, 'a[href*="qryCmpyDetail"]'),
        (By.CSS_SELECTOR, 'table.table a'),
        (By.CSS_SELECTOR, '.resultList a'),
    ]
    link = find_first(driver, candidates)
    if not link:
        raise RuntimeError("找不到查詢結果，請確認統編是否正確")

    handles_before = set(driver.window_handles)
    link.click()
    time.sleep(2.0)

    # 新分頁？
    handles_after = set(driver.window_handles)
    new_handles = handles_after - handles_before
    if new_handles:
        driver.switch_to.window(next(iter(new_handles)))

    time.sleep(1.5)
    wait_through_cloudflare(driver, headed=headed)


# ---------------------------------------------------------------------------
# Tabs & PDF
# ---------------------------------------------------------------------------
def discover_tabs(driver: webdriver.Chrome):
    selectors = [
        (By.CSS_SELECTOR, '[role="tab"]'),
        (By.CSS_SELECTOR, 'ul.nav-tabs a'),
        (By.CSS_SELECTOR, 'ul.nav a'),
        (By.CSS_SELECTOR, '.tab a'),
        (By.CSS_SELECTOR, '.tabs a'),
    ]
    for by, sel in selectors:
        elems = driver.find_elements(by, sel)
        if len(elems) >= 2:
            return elems

    found = []
    for label in KNOWN_TAB_LABELS:
        try:
            el = driver.find_element(By.XPATH, f"//a[normalize-space()='{label}']")
            found.append(el)
        except NoSuchElementException:
            try:
                el = driver.find_element(By.XPATH, f"//a[contains(., '{label}')]")
                found.append(el)
            except NoSuchElementException:
                continue
    return found


def get_company_name(driver: webdriver.Chrome, fallback: str) -> str:
    selectors = [
        (By.TAG_NAME, "h1"),
        (By.TAG_NAME, "h2"),
        (By.CSS_SELECTOR, ".cmpyName"),
        (By.CSS_SELECTOR, ".title"),
        (By.XPATH, "//td[contains(., '公司名稱')]/following-sibling::td[1]"),
        (By.XPATH, "//th[contains(., '公司名稱')]/following-sibling::td[1]"),
    ]
    for by, sel in selectors:
        try:
            txt = driver.find_element(by, sel).text.strip()
            if txt and len(txt) < 80:
                return txt
        except NoSuchElementException:
            continue
    return fallback


def save_pdf(driver: webdriver.Chrome, out_path: Path) -> None:
    """用 Chrome DevTools 的 Page.printToPDF 出 PDF (與瀏覽器列印一致)。"""
    out_path.parent.mkdir(parents=True, exist_ok=True)
    result = driver.execute_cdp_cmd("Page.printToPDF", {
        "printBackground": True,
        "paperWidth": 8.27,    # A4 inches
        "paperHeight": 11.69,
        "marginTop": 0.4,
        "marginBottom": 0.4,
        "marginLeft": 0.3,
        "marginRight": 0.3,
        "preferCSSPageSize": False,
    })
    out_path.write_bytes(base64.b64decode(result["data"]))


def download_all_tabs(driver: webdriver.Chrome, out_dir: Path, prefix: str) -> int:
    tabs = discover_tabs(driver)
    if not tabs:
        save_pdf(driver, out_dir / f"{prefix}.pdf")
        print(f"[saved] {prefix}.pdf  (沒偵測到頁籤，整頁輸出)")
        return 1

    saved = 0
    seen: set[str] = set()
    for i, tab in enumerate(tabs):
        try:
            label = (tab.text or "").strip()
        except Exception:
            label = f"tab_{i}"
        if not label or label in seen:
            continue
        seen.add(label)

        try:
            driver.execute_script("arguments[0].scrollIntoView({block:'center'});", tab)
            tab.click()
        except Exception as e:
            print(f"[skip] 點不到頁籤「{label}」: {e}")
            continue

        time.sleep(0.8)  # 等 AJAX

        out_file = out_dir / f"{prefix}_{saved + 1:02d}_{slugify(label)}.pdf"
        try:
            save_pdf(driver, out_file)
            print(f"[saved] {out_file.name}")
            saved += 1
        except Exception as e:
            print(f"[fail ] {label}: {e}")
    return saved


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------
def main() -> int:
    ap = argparse.ArgumentParser(description="findbiz 自動查詢並下載各頁籤 PDF (Selenium 版)")
    ap.add_argument("tax_id", help="統一編號，例如 04595257")
    ap.add_argument("-o", "--out", default="output", help="PDF 輸出根目錄 (預設 ./output)")
    ap.add_argument("--headed", action="store_true", help="顯示瀏覽器 (建議第一次使用)")
    ap.add_argument("--timeout", type=int, default=30000, help="頁面載入 timeout (ms)")
    ap.add_argument("--state", default="findbiz_cookies.json",
                    help="存放 cookies 的 JSON 檔；通過 Cloudflare 後會自動寫入")
    ap.add_argument("--user-agent", default=DEFAULT_UA, help="自訂 User-Agent")
    args = ap.parse_args()

    if not re.fullmatch(r"\d{8}", args.tax_id):
        print("警告：統編應為 8 位數字", file=sys.stderr)

    driver = build_driver(args)
    try:
        load_cookies(driver, Path(args.state))

        print(f"→ 開啟查詢頁面，搜尋統編 {args.tax_id}")
        fill_tax_id_and_search(driver, args.tax_id, headed=args.headed)

        print("→ 點開第一筆查詢結果")
        open_first_result(driver, args.tax_id, headed=args.headed)

        company = get_company_name(driver, args.tax_id)
        print(f"→ 公司：{company}")

        out_dir = Path(args.out) / f"{args.tax_id}_{slugify(company)}"
        count = download_all_tabs(driver, out_dir, args.tax_id)
        print(f"\n完成，共輸出 {count} 個 PDF 至 {out_dir}")

        save_cookies(driver, Path(args.state))
        return 0
    except Exception as e:
        print(f"\n發生錯誤：{e}", file=sys.stderr)
        return 1
    finally:
        try:
            driver.quit()
        except Exception:
            pass


if __name__ == "__main__":
    sys.exit(main())
