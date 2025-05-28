# L Language

L is a very simple untyped functional programming language based on lambda calculus for evaluating simple math expressions.

This project puts a web interface to the interpreter. 

## 🚀 Project Setup & Run Guide

This project contains:
- ✅ A **Haskell Scotty server** (`l-lang`)
- ✅ A **Vite + React client** (`web-client`)
- ✅ Docker + Docker Compose configuration to run both together

---
## 📦 Project Structure

```
./            → Haskell Scotty app (with Dockerfile)
/web-client        → Vite + React app (with Dockerfile)
docker-compose.yml → top-level orchestration
```
---
## 🔧 Prerequisites

* Install **Docker** → [https://docs.docker.com/get-docker/](https://docs.docker.com/get-docker/)
* Install **Docker Compose** → included in recent Docker Desktop versions
---

## 🏗 How to Build & Run

1️⃣ Clone the repository:

```bash
git clone https://github.com/claeusdev/l-lang
cd l-lang
```

2️⃣ Build and start everything:

```bash
docker compose up --build
```

✅ This will:

* Build the **server** from the Haskell Dockerfile.
* Build the **client** from the React Dockerfile.
* Expose:

  * **Server** → [http://localhost:3000](http://localhost:3000)
  * **Client (Vite dev server)** → [http://localhost:5173](http://localhost:5173)

---

## 🌐 Access the Apps

| Service | Local URL                                      |
| ------- | ---------------------------------------------- |
| Server  | [http://localhost:3000](http://localhost:3000) |
| Client  | [http://localhost:5173](http://localhost:5173) |

---

## 🔄 Common Commands

* **Stop the services:**

  ```bash
  docker-compose down
  ```

* **Rebuild only:**

  ```bash
  docker-compose build
  ```

* **View logs:**

  ```bash
  docker-compose logs -f
  ```

* **Restart with rebuild:**

  ```bash
  docker-compose up --build
  ```
---
