# LipidSig 2.0 — Docker 部署技術報告

## 1. 背景與目標

LipidSig 2.0 是一套以 R Shiny 開發的脂質體學分析平台，涵蓋 Profiling、Differential Expression、Data Check、Correlation、Machine Learning、Network、Enrichment 及 ID Conversion 等八個分析模組。為了讓一般使用者（無論是否具備 R/Bioinformatics 環境）都能在本機執行完整的分析流程，本報告說明如何透過 Docker 將 LipidSig 打包並對外發布。

---

## 2. 應用程式架構：多模組合併為單一 Shiny App

### 2.1 模組化設計

LipidSig 採用「主框架 + 子模組」的架構，每個分析頁面各自擁有一對 `ui_*.R` / `server_*.R` 檔案：

```
ui.R                ← 主 UI（navbarPage 整合所有分頁）
server.R            ← 主 Server（source 所有 server_*.R）
ui_home.R
ui_DataCheck.R      ↘
ui_Profiling.R        各頁面 UI 定義
ui_DE.R             ↗
ui_Correlation.R
ui_ML.R
ui_Network.R
ui_Enrichment.R
ui_IDconversion.R
server_DataCheck.R  ↘
server_Profiling.R    各頁面 Server 邏輯
server_DE.R         ↗
server_Correlation.R
server_ML.R
server_Network.R
server_Enrichment.R
server_IDconversion.R
```

### 2.2 合併策略

所有子模組在 `ui.R` 的 `navbarPage()` 中以獨立 `tabPanel` 嵌入，構成一個完整的單頁應用（SPA）。`server.R` 以 `shiny::reactiveValues()` 維護一組跨模組共享的狀態物件（`variables`），確保各分析頁面之間的資料傳遞與流程銜接。

此設計的優點：
- **單一進入點**：使用者只需存取一個 URL（`http://localhost:3838/`）即可使用全部功能
- **狀態共享**：Data Check 上傳並驗證的資料，可直接流通至 Profiling、DE、ML 等後續分析
- **部署簡便**：整個 app 只需啟動一個 Shiny Server 實例

---

## 3. Docker 化流程

### 3.1 基礎映像

```dockerfile
FROM --platform=linux/amd64 rocker/shiny:4.4.1
```

選用 [rocker/shiny](https://rocker-project.org/) 作為基礎，原因：
- 已預裝 R 4.4.1 與 Shiny Server
- 維護積極、與 CRAN/Bioconductor 相容性佳
- `--platform=linux/amd64` 確保在 Apple Silicon (M1/M2/M3) Mac 上行為一致

### 3.2 系統相依套件

Dockerfile 第一層安裝所有必要的系統函式庫（`libssl-dev`、`libxml2-dev`、`libgit2-dev`、`default-jdk` 等），讓後續 R 套件能正常編譯。

### 3.3 R 套件分層安裝

R 套件依功能分為多個 `RUN` 指令（Shiny UI、資料處理、視覺化、機器學習、Bioconductor），充分利用 Docker 的 **layer cache**：修改某一群組時，只有該層以後的層需要重新建置，大幅縮短迭代時間。

### 3.4 應用程式複製

```dockerfile
COPY . /srv/shiny-server/lipidsig/
```

搭配 `.dockerignore` 排除以下非必要內容，減少映像體積並加速建置：

| 排除項目 | 說明 |
|---|---|
| `.git/` | Git 版本控制資料，容器內不需要 |
| `**/.DS_Store` | macOS 系統產生的元資料 |
| `logs/` | 執行期才寫入，掛載自主機 |
| `www/tmp/` | 執行期產生的 KEGG pathway 暫存圖 |
| `www/Documentation/*.webp` | 未被程式碼引用的文件截圖 |

### 3.5 Shiny Server 設定

`shiny-server.conf` 覆蓋預設設定，將 `/lipidsig` 路由指向應用程式目錄，並開放 3838 埠。

---

## 4. Docker Compose 部署設定

`docker-compose.yml` 提供一鍵啟動的完整設定：

```yaml
services:
  lipidsig:
    platform: linux/amd64
    build:
      context: .
      dockerfile: Dockerfile
    image: lipidsig:2.0
    container_name: lipidsig_app
    ports:
      - "3838:3838"
    volumes:
      - ./logs:/var/log/shiny-server   # 日誌掛載至主機，便於監控
    restart: unless-stopped
    environment:
      - SHINY_LOG_LEVEL=WARN
```

`restart: unless-stopped` 確保容器在主機重新開機後自動恢復，適合長期運行的研究伺服器環境。

---

## 5. 發布流程

```
1. 開發者將程式碼推送至 GitHub repository
         ↓
2. 使用者安裝 Docker Desktop（Windows/macOS/Linux）
         ↓
3. git clone <repository-url>
         ↓
4. cd LipidSig && docker compose up -d --build
         ↓
5. 瀏覽器開啟 http://localhost:3838/
```

首次建置約需 **30–60 分鐘**（主要是編譯 R 套件與 Bioconductor 套件）。建置完成後，後續啟動僅需數秒（直接使用已快取的映像）。

---

## 6. 效益摘要

| 面向 | 傳統安裝 R 環境 | Docker 部署 |
|---|---|---|
| 環境一致性 | 依賴使用者系統，容易有版本衝突 | 完全一致，跨平台相同行為 |
| 安裝難度 | 需手動安裝 R、Bioconductor、系統函式庫 | 只需安裝 Docker，一行指令啟動 |
| 維護成本 | 使用者需自行更新套件 | 開發者更新映像後，使用者重新 pull 即可 |
| 可重現性 | 難以保證 | 由 Dockerfile 鎖定所有版本 |

---

## 7. 未來擴充方向

- **Docker Hub / GitHub Container Registry**：推送預建映像，讓使用者跳過 build 步驟，直接 `docker compose pull && docker compose up`
- **反向代理（Nginx）**：部署於公開伺服器時，加入 Nginx 作為前端以支援 HTTPS 與路徑路由
- **資源限制**：在 `docker-compose.yml` 中加入 `deploy.resources.limits` 控制 CPU/記憶體上限，防止大型資料集分析耗盡伺服器資源
