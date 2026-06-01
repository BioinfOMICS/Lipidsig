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
- [Linux (Ubuntu / Debian / Fedora)](https://docs.docker.com/desktop/install/linux-install/)

After installing, "open Docker Desktop" and wait until the whale icon in your taskbar turns green - this means Docker is ready. 

Confirm it's working:

```bash
docker --version
docker compose version
```

Both commands should print a version number without errors. 

### Git

Git is used to download the LipidSig repository.

| OS | How to install |
|---|---|
| Windows | Download from [git-scm.com](https://git-scm.com/download/win) and run the installer |
| macOS | Run `git --version` in Terminal — macOS will prompt you to install it |
| Linux | `sudo apt install git` (Debian/Ubuntu) or `sudo dnf install git` (Fedora) |

---

## Installation

**1. Clone the repository**

```bash
git clone https://github.com/bioinfomics/LipidSig.git
cd LipidSig
```

**2. Start the app**

```bash
docker compose up -d
```

The first time you run this, Docker will download the pre-built image from Docker Hub (a few minutes depending on your connection). After that, it starts in seconds.
 
**3. Open LipidSig in your browser**

Wait a few moments for the container to finish starting up, then navaigate to:

```
http://localhost:3838/
```
 
You should see the LipidSig 2.0 interface. If you get a "site can't be reached" error, the container is still starting - wait a moment and refresh. 

---

## Stopping and Restarting
 
| Action | Command |
|---|---|
| Pause the app (keeps the container) | `docker compose stop` |
| Resume a paused app | `docker compose start` |
| Stop and remove the container | `docker compose down` |
 
---

## Updating LipidSig

**Method 1 — Pre-built image (recommended)**
 
Downloads the latest image from Docker Hub. No compilation required.
 
```bash
git pull
docker compose pull
docker compose up -d
```
 
**Method 2 — Build from source**
 
Use this if you've made local changes to the source code.
 
```bash
git pull
docker compose -f docker-compose.build.yml up -d --build
```

---

## Troubleshooting
 
**"Cannot connect to the Docker daemon"**
 
Docker Desktop is not running. Open it and wait for the whale icon to turn green, then try again.
 
---
 
**App not loading at localhost:3838**
 
Check whether the container is running:
 
```bash
docker compose ps
```
 
If it's running but the page won't load, check the logs for errors:
 
```bash
docker compose logs -f
```

---

**Port 3838 is already in use**
 
Open `docker-compose.yml` and change the left side of the port mapping:
 
```yaml
ports:
  - "3939:3838"   # change 3939 to any free port you prefer
```
 
Then restart:
 
```bash
docker compose up -d
```
 
Access the app at `http://localhost:3939/` (or whichever port you chose).

---

**Slow performance on Apple Silicon (M1/M2/M3)**

The image is built for `linux/amd64`. On Apple Silicon, Docker runs it through Rosetta 2 emulation, which can slow down CPU-intensive analyses. This is expected and not a bug.

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

> *(Citation information to be added)*

---

## License

*(License information to be added)*
