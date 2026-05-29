# LipidSig 2.0

A comprehensive web-based platform for lipidomics data analysis, covering Profiling, Differential Expression, Data Check, Correlation, Machine Learning, Network, Enrichment, and ID Conversion.

> **Running LipidSig locally requires only Docker — no R installation needed.**

---

## Prerequisites

### Step 1 — Install Docker Desktop

Docker Desktop bundles the Docker Engine and the `docker compose` command. Install it for your operating system:

| OS | Download |
|---|---|
| **Windows 10/11** | [https://docs.docker.com/desktop/install/windows-install/](https://docs.docker.com/desktop/install/windows-install/) |
| **macOS (Intel & Apple Silicon)** | [https://docs.docker.com/desktop/install/mac-install/](https://docs.docker.com/desktop/install/mac-install/) |
| **Linux (Ubuntu / Debian / Fedora)** | [https://docs.docker.com/desktop/install/linux-install/](https://docs.docker.com/desktop/install/linux-install/) |

After installation, **start Docker Desktop** and wait until the whale icon in your taskbar/menu bar turns green (this means the Docker Engine is running).

Verify the installation by opening a terminal and running:

```bash
docker --version
docker compose version
```

Both commands should print version numbers without errors.

---

### Step 2 — Install Git

Git is needed to download the repository.

| OS | Instructions |
|---|---|
| **Windows** | Download from [https://git-scm.com/download/win](https://git-scm.com/download/win) and run the installer |
| **macOS** | Run `git --version` in Terminal — macOS will prompt to install Xcode Command Line Tools if Git is absent |
| **Linux** | `sudo apt install git` (Debian/Ubuntu) or `sudo dnf install git` (Fedora) |

---

## Installation & Launch

### Step 3 — Clone the repository

Open a terminal (Command Prompt / PowerShell on Windows, Terminal on macOS/Linux) and run:

```bash
git clone https://github.com/<YOUR_USERNAME>/LipidSig.git
cd LipidSig
```

> Replace `<YOUR_USERNAME>` with the actual GitHub username or organisation.

---

### Step 4 — Build and start the app

```bash
docker compose up -d --build
```

What this does:

- `--build` builds the Docker image from the `Dockerfile` (only needed the first time, or after code updates)
- `-d` runs the container in the background (detached mode)

> **⏱ First-time build:** Expect **30–60 minutes** while Docker downloads the base image and compiles all R and Bioconductor packages. Subsequent launches take only a few seconds.

---

### Step 5 — Open LipidSig in your browser

Once the build is complete, open:

```
http://localhost:3838/
```

You should see the LipidSig 2.0 interface.

---

## Stopping and Restarting

| Action | Command |
|---|---|
| Stop the app (keep the container) | `docker compose stop` |
| Stop and remove the container | `docker compose down` |
| Restart a stopped container | `docker compose start` |
| Rebuild after a code update | `docker compose up -d --build` |

---

## Updating LipidSig

When a new version is released:

```bash
# Pull the latest code
git pull

# Rebuild and restart
docker compose up -d --build
```

---

## Troubleshooting

### "Cannot connect to the Docker daemon"

Docker Desktop is not running. Open Docker Desktop and wait for the engine to start (green icon), then retry.

### App not loading at localhost:3838

Check whether the container is running:

```bash
docker compose ps
```

View the application logs for errors:

```bash
docker compose logs -f
```

### Port 3838 is already in use

Edit `docker-compose.yml` and change the left side of the port mapping:

```yaml
ports:
  - "3939:3838"   # access via http://localhost:3939/
```

Then run `docker compose up -d` again.

### Slow performance on Apple Silicon (M1/M2/M3)

The image is built for `linux/amd64`. Docker Desktop uses Rosetta 2 emulation on Apple Silicon, which may reduce performance for CPU-intensive analyses. This is expected behaviour.

---

## System Requirements

| Resource | Minimum | Recommended |
|---|---|---|
| RAM | 4 GB | 8 GB or more |
| Free disk space | 8 GB | 15 GB (for image + data) |
| CPU | Dual-core | Quad-core or more |

---

## File Upload Limit

LipidSig accepts files up to **30 MB** per upload. If your dataset exceeds this limit, contact the maintainers or edit the `options(shiny.maxRequestSize = ...)` line in `server.R` before rebuilding.

---

## Citation

If you use LipidSig in your research, please cite:

> *(Citation information to be added)*

---

## License

*(License information to be added)*
