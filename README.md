# LipidSig 2.0

**LipidSig 2.0** is a comprehensive web-based platform for lipidomics data analysis. It provides an integrated workflow for lipidomics data processing, statistical analysis, visualization, enrichment analysis, machine learning, network analysis, correlation analysis, data quality checking, and lipid identifier conversion.

> **You only need Docker to run LipidSig locally. No R, RStudio, or R packages required.**

---

## Requirements

Before you start, make sure you have both of the following installed:

### Docker Desktop

Docker Desktop runs the application inside a container.

- [Windows](https://docs.docker.com/desktop/install/windows-install/)
- [macOS (Intel & Apple Silicon)](https://docs.docker.com/desktop/install/mac-install/)

After installing, open Docker Desktop and wait until it indicates that the Docker engine is running.

Then open PowerShell, Command Prompt, or Terminal and run:

```bash
docker --version
docker compose version
```

Both commands should display a version number without errors.
 
### Git

Git is used to download the LipidSig repository.

| OS | How to install |
|---|---|
| Windows | Download from [git-scm.com](https://git-scm.com/download/win) and run the installer |
| macOS | Run `git --version` in Terminal. macOS will prompt you to install the command-line tools if Git is not already available |

---

## Installation

### 1. Clone the repository

Open PowerShell, Command Prompt, or Terminal and run:

``` bash
git clone https://github.com/bioinfomics/LipidSig.git
cd LipidSig
```

### 2. Start the application

``` bash
docker compose up -d
```

The first time this command is run, Docker downloads the pre-built LipidSig image from Docker Hub. The download may take several minutes, depending on your internet connection. Subsequent starts should be much faster.

### 3. Open LipidSig in your browser

Wait a few moments for the container to finish starting, and then navigate to:

``` text
http://localhost:3838/
```

The LipidSig 2.0 interface should appear.

If the browser displays a “site cannot be reached” message, the container may still be starting. Wait briefly and refresh the page.

---

## Stopping and Restarting
 
| Action | Command |
|---|---|
| Pause the application while keeping the container | `docker compose stop` |
| Resume a paused application | `docker compose start` |
| Stop and remove the container | `docker compose down` |
 
---

## Updating LipidSig

**Method 1 — Pre-built image (recommended)**
 
This method downloads the latest LipidSig image from Docker Hub. No compilation is required.
 
```bash
git pull
docker compose pull
docker compose up -d
```
 
**Method 2 — Build from source**
 
Use this method if you have made local changes to the LipidSig source code.
 
```bash
git pull
docker compose -f docker-compose.build.yml up -d --build
```

---

## Troubleshooting
 
### 1. “Cannot connect to the Docker daemon”

Docker Desktop is not running or has not finished starting.

Open Docker Desktop and wait until it indicates that the Docker engine is running. Then retry:

```bash
docker compose up -d
```

You can also test Docker with:

```bash
docker info
```

If Docker is working correctly, this command should display information about the Docker client and server.

### 2. LipidSig does not load at `localhost:3838`

Check whether the container is running:

```bash
docker compose ps
```

If the container is running but the page does not load, inspect the logs:

```bash
docker compose logs -f
```

Press `Ctrl+C` to stop following the logs.

You can also restart the application:

```bash
docker compose down
docker compose up -d
```

### 3. Port 3838 is already in use

Open `docker-compose.yml` and locate the port mapping:

```yaml
ports:
  - "3838:3838"
```

Change the number on the left to another available port. For example:

```yaml
ports:
  - "3939:3838"
```

Restart the application:

```bash
docker compose down
docker compose up -d
```

Then access LipidSig at:

```text
http://localhost:3939/
```

Replace `3939` with the port number you selected.

### 4. Slow performance on Apple Silicon

The Docker image may run through architecture emulation on Apple Silicon computers. This can make CPU-intensive analyses slower than they are on systems that use the image’s native architecture.

This behavior is expected and does not necessarily indicate a problem with LipidSig.

### 5. Windows error: "There was a problem with WSL"

Docker Desktop on Windows commonly uses the Windows Subsystem for Linux 2, or WSL 2, backend. This error may occur when WSL is outdated, stopped, incorrectly configured, or missing required Windows features.

#### Step 1 - Close Docker Desktop

Completely exit Docker Desktop before running the following commands.

#### Step 2 — Update and restart WSL

Open **PowerShell as Administrator** and run:

```powershell
wsl --update
wsl --shutdown
```

Restart Windows, open Docker Desktop, and try starting LipidSig again:

```bash
docker compose up -d
```

#### Step 3 — Check the WSL status

In PowerShell, run:

```powershell
wsl --status
wsl --version
wsl --list --verbose
```

The output should show that WSL is installed and that the relevant distributions use WSL version 2.

Set WSL 2 as the default for future distributions:

```powershell
wsl --set-default-version 2
```

If an installed distribution is using WSL 1, convert it with:

```powershell
wsl --set-version <distribution-name> 2
```

Replace `<distribution-name>` with the name shown by:

```powershell
wsl --list --verbose
```

#### Step 4 — Enable the required Windows features

Search for **Turn Windows features on or off** from the Windows Start menu.

Make sure the following options are enabled:

- **Windows Subsystem for Linux**
- **Virtual Machine Platform**

Select **OK** and restart Windows.

Alternatively, open PowerShell as Administrator and run:

```powershell
dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart
```

#### Step 5 — Check hardware virtualization

Open **Task Manager**, select **Performance**, and then select **CPU**.

Confirm that the following is displayed:

```text
Virtualization: Enabled
```

If virtualization is disabled, enable Intel VT-x, Intel Virtualization Technology, AMD-V, or SVM Mode in the computer’s BIOS or UEFI settings. The name and location of this setting vary by computer manufacturer.

#### Step 6 — Check the Docker Desktop WSL settings

Open Docker Desktop and go to:

```text
Settings → General
```

Make sure **Use the WSL 2 based engine** is enabled.

Then go to:

```text
Settings → Resources → WSL Integration
```

Enable WSL integration for the appropriate distribution, and select **Apply & restart**.

#### Step 7 — Reinstall WSL if it is missing

If commands such as `wsl --status` report that WSL is not installed, open PowerShell as Administrator and run:

```powershell
wsl --install
```

Restart Windows when prompted. Then run:

```powershell
wsl --update
wsl --set-default-version 2
```

Open Docker Desktop and retry the LipidSig installation.

#### Step 8 — Restart Docker Desktop

After repairing WSL, restart Docker Desktop and verify that Docker is working:

```bash
docker --version
docker compose version
docker info
```

Then return to the LipidSig directory and run:

```bash
docker compose up -d
```

If the WSL error continues, update Windows and Docker Desktop to their latest available versions. Docker Desktop’s troubleshooting or diagnostic tools can also be accessed from the Docker Desktop **Troubleshoot** menu.

---

## System Requirements

| Resource | Minimum | Recommended |
|---|---|---|
| RAM | 4 GB | 8 GB or more |
| Free disk space | 8 GB | 15 GB (for image + data) |
| CPU | Dual-core | Quad-core or more |

---

## File Upload Limit

LipidSig accepts files up to **30 MB** per upload. If your dataset exceeds this limit, contact the maintainers or edit the `options(shiny.maxRequestSize = ...)` line in `server.R` and rebuild the image.

---

## Citation

If you use LipidSig in your research, please cite:

Chia-Hsin Liu, Pei-Chun Shen, Wen-Jen Lin, Hsiu-Cheng Liu, Meng-Hsin Tsai, Tzu-Ya Huang, I-Chieh Chen, Yo-Liang Lai, Yu-De Wang, Mien-Chie Hung, Wei-Chung Cheng, LipidSig 2.0: integrating lipid characteristic insights into advanced lipidomics data analysis, Nucleic Acids Research, Volume 52, Issue W1, 5 July 2024, Pages W390–W397, doi: <a href="https://doi.org/10.1093/nar/gkae335" target="_blank">10.1093/nar/gkae335</a>. PMID: 38709887.

Chia-Hsin Liu, Pei-Chun Shen, Wen-Jen Lin, Hsiu-Cheng Liu, Meng-Hsin Tsai, Yo-Liang Lai, Yu-De Wang, Mien-Chie Hung, Wei-Chung Cheng, LipidSigR: a R-based solution for integrated lipidomics data analysis and visualization, Bioinformatics Advances, Volume 5, Issue 1, 2025, vbaf047, doi: <a href="https://doi.org/10.1093/bioadv/vbaf047" target="_blank">10.1093/bioadv/vbaf047</a>. PMID: 40110562.

