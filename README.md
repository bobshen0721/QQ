# QQ

## findbiz 公司資料自動下載

輸入統一編號 (統編)，自動到 [商工登記公示資料查詢服務](https://findbiz.nat.gov.tw/fts/query/QueryBar/queryInit.do) 查詢，
點開查詢結果第一筆公司，把每個頁籤分別存成 PDF。

### 安裝

```bash
pip install -r requirements.txt
python -m playwright install chromium
```

### 使用

```bash
python findbiz_pdf.py 04595257                 # 預設輸出到 ./output
python findbiz_pdf.py 04595257 -o ~/Downloads  # 自訂輸出位置
python findbiz_pdf.py 04595257 --headed        # 顯示瀏覽器 (除錯)
```

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
