# QQ

## findbiz 公司資料自動下載

輸入統一編號 (統編)，自動到 [商工登記公示資料查詢服務](https://findbiz.nat.gov.tw/fts/query/QueryBar/queryInit.do) 查詢，
點開查詢結果第一筆公司，把每個頁籤分別存成 PDF。

提供兩種版本，挑一種用即可：

| 版本 | 入口檔 | 安裝 |
| --- | --- | --- |
| Playwright | `findbiz_pdf.py` | `pip install playwright && python -m playwright install chrome` |
| **Selenium** (含反偵測) | `findbiz_pdf_selenium.py` | `pip install selenium undetected-chromedriver` (需本機已裝 Chrome) |

### 安裝

```bash
pip install -r requirements.txt
python -m playwright install chromium
# 強烈建議再裝本機 Chrome，可大幅降低被 Cloudflare 攔下的機率：
python -m playwright install chrome
```

### 使用 (Playwright)

```bash
python findbiz_pdf.py 04595257 --headed   # 第一次：必要時手動過 Cloudflare
python findbiz_pdf.py 04595257            # 之後 headless 直接跑
python findbiz_pdf.py 04595257 -o ~/Downloads
python findbiz_pdf.py 04595257 --channel chrome
```

### 使用 (Selenium + undetected-chromedriver)

```bash
python findbiz_pdf_selenium.py 04595257 --headed   # 第一次
python findbiz_pdf_selenium.py 04595257            # 之後
python findbiz_pdf_selenium.py 04595257 -o ~/Downloads
python findbiz_pdf_selenium.py 04595257 --state my_cookies.json
```

Selenium 版重點：
- 預設用 **undetected-chromedriver**，已自動處理 `navigator.webdriver` 等指紋
  (沒裝會退回原生 Selenium，但較容易被擋)
- PDF 透過 Chrome DevTools `Page.printToPDF` 產生，效果與瀏覽器列印一致
- 通過驗證後 cookies 存到 `findbiz_cookies.json`，下次自動帶入

### Cloudflare 驗證

findbiz 在前面掛了 Cloudflare bot challenge，原始 Playwright 一定會被擋。
本腳本已做好以下處理：

1. 優先用本機 **Google Chrome**（`--channel chrome`），不用 Chrome for Testing
2. 啟動時移除 `--enable-automation`、注入 stealth script 蓋掉 `navigator.webdriver`
3. 偵測到驗證頁時：`--headed` 會等你手動點通過，無頭模式會直接報錯提示
4. 通過驗證後把 cookies 存到 `findbiz_state.json`，下次自動載入

如果還是被擋，最務實的做法：用 `--headed` 跑一次 → 手動點過 → 之後用 headless。

輸出檔名格式：

```
output/<統編>_<公司名稱>/<統編>_01_基本資料.pdf
                          <統編>_02_董監事資料.pdf
                          ...
```

### 備註

- PDF 由 Chromium headless 直接列印，效果與瀏覽器 Ctrl+P → 另存 PDF 相同。
- 頁籤偵測使用多組備援選擇器；若官方網站改版，調整 `findbiz_pdf.py` 內的
  `KNOWN_TAB_LABELS` 與 `discover_tabs()` 的選擇器即可。
- 預設 headless；若查詢失敗想看實際畫面，加 `--headed`。
