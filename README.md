# QQ

## findbiz 公司資料自動下載

輸入統一編號 (統編)，自動到 [商工登記公示資料查詢服務](https://findbiz.nat.gov.tw/fts/query/QueryBar/queryInit.do) 查詢，
點開查詢結果第一筆公司，把每個頁籤分別存成 PDF。

### 安裝

```bash
pip install -r requirements.txt
python -m playwright install chromium
# 強烈建議再裝本機 Chrome，可大幅降低被 Cloudflare 攔下的機率：
python -m playwright install chrome
```

### 使用

```bash
# 第一次：建議用 --headed，若被 Cloudflare 攔下手動點過驗證，
# 通過後會自動把 session 寫到 findbiz_state.json
python findbiz_pdf.py 04595257 --headed

# 之後正常跑 (帶上次的 session，通常免再驗證)
python findbiz_pdf.py 04595257

python findbiz_pdf.py 04595257 -o ~/Downloads          # 自訂輸出位置
python findbiz_pdf.py 04595257 --channel chrome        # 強制用本機 Chrome
python findbiz_pdf.py 04595257 --state my_session.json # 自訂 session 檔
```

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
