# Go‑Terminal‑App – Project Spec   v0.1 (2025‑05‑06)

## 1 Purpose
Play a full (19 × 19) game of *Go* between two computers on the **same LAN**, entirely in the terminal.  
The UI is built with **Brick 2 + Vty 6**, the game engine is **pure Haskell** (no `IO`), and the two peers exchange **JSON‑encoded game‑state messages** over a single TCP socket.

---

## 2 Gameplay Rules

| Item | Choice | Rationale |
|------|--------|-----------|
| Board size | **19 × 19** only | keeps UI simple |
| Komi | configurable later (default = 7.5) |
| Scoring | **Chinese counting** (area) |
| Ko / super‑ko | basic ko only, no super‑ko for v0.1 |
| Result | Black / White win, or **Tie** if scores equal |
| Commands | *Move* (row,col) • *Pass* • *Resign* |
| Engine purity | **Pure** – engine lives in `Game.*`, returns new `GameState` |

---

## 3 Networking (Model + Protocol)

* **Peer‑to‑peer**: every instance does a `listen` on a fixed port (50555 by default) **and** tries to dial the address it was given.  
  The socket that succeeds first decides colors:  
  * **Dial → Black**, **Listener → White**.
* Connection hand‑shake (JSON, UTF‑8, `\n` terminated):

  ```jsonc
  // client → server
  { "tag": "HELLO", "nick": "Alice", "boardSize": 19 }

  // server → client (accept)
  { "tag": "WELCOME", "yourColor": "White" }
  // or (reject)
  { "tag": "BUSY" }
# Go‑Terminal‑App — Technical Specification (v0.2 / 2025‑05‑06)

All statements below describe concrete behaviour that **will be implemented in Haskell (GHC ≥ 9.4)**.  
Nothing here depends on external services or GUI toolkits besides the listed libraries.

---

## 1 High‑level Overview

| Concern      | Approach (Haskell‑level)                    | Main module(s)        |
|--------------|---------------------------------------------|-----------------------|
| Pure rules   | Functional engine, no `IO`                  | `Game.Engine`, `Game.Types` |
| Persistence  | _None_ — state kept in RAM only             | — |
| Networking   | `network` sockets + `aeson` JSON encoding   | `Network.TCP`, `Network.WireMsg` |
| Terminal UI  | **Brick ≥ 2.0** on **Vty ≥ 6.0**            | `UI.*`                |
| Concurrency  | One lightweight thread (`async`) to read the socket; UI runs in the main thread. | `Network.TCP`, `Main.hs` |

---

## 2 Game Logic (Engine)

* **Board**: fixed `19 × 19` grid.  Indexing is `(row :: Int, col :: Int)` with both ranging `0‥18`.
* **Data types**

  ```haskell
  data Stone      = Empty | Black | White           deriving (Eq, Show)
  data Player     = PBlack | PWhite                deriving (Eq, Show)
  data Move       = Play Int Int | Pass | Resign   deriving (Eq, Show)
  type Board      = Vector Stone                   -- length 361
  data GameState  = GS
      { board        :: !Board
      , toMove       :: !Player      -- whose turn
      , captured     :: !(Int,Int)   -- (blackCaps, whiteCaps)
      , seqNo        :: !Word64      -- monotonic, starts at 0
      }
  ```

* **Chinese scoring**: `score = stones_on_board + captures`.
* **Ko**: simple ko (cannot recreate **exactly** previous position).

_All pure functions:_

```haskell
initialState   :: GameState
applyMove      :: GameState -> Move -> Either RuleViolation GameState
chineseScore   :: GameState -> (Int,Int)    -- (BlackScore, WhiteScore)
winner         :: GameState -> Result       -- B | W | Tie | Undecided
```

---

## 3 Wire Protocol (JSON over TCP)

* **Line‑delimited** UTF‑8 JSON; each message on its own line.
* Messages (shared `data` + `DeriveGeneric`, `Aeson`):

  ```haskell
  data WireMsg
    = Hello   { nick :: Text }
    | Welcome { yourColor :: Player }
    | Busy
    | State   { gs   :: GameState }   -- full authoritative state
    | Chat    { txt  :: Text }        -- (reserved, not used v0.2)
    deriving (Generic, ToJSON, FromJSON)
  ```

* **Session flow**

  ```
  listener waits ─┐
                  ├─> (dialer)  Hello
                  │             Welcome {Black}
                  │             State  {seq=0}
                  └─>            … regular State updates
  If port already taken: listener responds Busy, dialer exits.
  ```

---

## 4 Network Implementation

* Port **50555** (configurable via `--port`).
* `Network.TCP.listen`  
  ```haskell
  listenLoop :: Int -> (Handle -> IO ()) -> IO ()
  ```
* `Network.TCP.dial`  
  ```haskell
  dial :: HostName -> Int -> IO (Either IOError Handle)
  ```
* Reading: `forever $ BS.hGetLine h >>= atomically . writeTQueue inbox`.
* Writing: `encode msg <> "\n"` via `BS.hPut h`.

* Only **one** connection lives at a time (peer‑to‑peer).

---

## 5 Terminal UI (Brick)

Screens                | Widget tree roots (names)
----------------------- | -------------------------
**MainMenu**           | `"menu"`
**Config** (host/port) | `"config"`
**Board**              | `"board"` (19×19 grid), `"overlay"` (status bar)

* `UI.Event` maps `BrickEvent` to pure actions, then `applyMove`.
* All screen transitions use `Brick.Main.run` with `continueWithoutRedraw` or `halt`.

Key bindings
```
← ↑ ↓ →   move cursor
SPACE     play stone
P         pass
R         resign
Q         quit
```

---

## 6 Build & Runtime Dependencies

| Package              | Version bound | Why |
|----------------------|---------------|-----|
| `base`               | 4.18 – 4.20   | GHC 9.4/9.6 |
| `aeson`              | ≥ 2           | JSON encode/decode |
| `text`, `vector`     | —             | core structures |
| `brick`              | **≥ 2.3**     | UI |
| `vty`                | ≥ 6           | terminal layer |
| `network`            | ≥ 3.1         | sockets |
| `async`              | ≥ 2.2         | background reader |

_No `network-simple` anymore._

---

## 7 Milestones

1. **Build green**  
   * compile with GHC 9.4, run HSpec unit tests.
2. **Play vs yourself (single machine)**  
   * two terminals, manual IP entry.
3. **Polish UI**  
   * better cursor, territory shading.
4. **Packaging**  
   * `stack install`, produce single static executable on macOS/Linux.

---

## 8 Out of Scope (for v0.2)

* Handicap, time control, AI opponent, SGF import/export, in‑game chat.

---

*Document history*: v0.1 (05‑06) initial, v0.2 (05‑06) clarify engine types, remove `network‑simple`, add milestones.