# MoArk (模力方舟) — MiniMax-H3 Video Generation API

Reference for calling the **MiniMax-H3** video generation model on MoArk's
Serverless API. Compiled from MoArk's official documentation (docs site,
OpenAPI-labelled model registry, and the public `gitee.com/moark/docs` source),
2026-09-29.

> Provenance: investigated by delegating the browser agent to MoArk's official
> docs (`moark.com/docs`) plus verification against the model-registry endpoint
> that backs the docs' interactive playground. MoArk only; no third-party docs.

## 1. Model overview (official)

> MiniMax H3 是 MiniMax 推出的通用全模态生成模型，可统一理解文本、图像、视频、音频多模态信息，原生同步生成带立体声的高清视频，现已开源，支持商用级多模态创作。

- Category: 视频生成 (Video Generation)
- Interfaces exposed on MoArk: **文生视频 (text-to-video)** and
  **首尾帧生视频 (first/last-frame-to-video)**.
- MiniMax-H3 service id `27370`; model id `28780`.

## 2. Endpoint, auth and base URLs

| Item | Value |
|------|-------|
| Submit (video) | `POST {API_URL}/v1/async/videos/generations` |
| Poll task | `GET {API_URL}/v1/task/{task_id}` (also `{API_URL}/api/v1/task/<task_id>`) |
| Task status | `GET {API_URL}/v1/task/<task_id>/status` |
| Cancel task | `POST {API_URL}/api/v1/task/<task_id>/cancel` |
| Available quota | `GET {API_URL}/v1/tasks/available-quota` |
| Auth | `Authorization: Bearer <ACCESS_TOKEN>` |
| Failover header | `X-Failover-Enabled: true` (optional; default `true`) |
| Webhook header | `X-WebHook: https://your.server/callback` (optional) |
| API format | `OPEN_AI` compatible; `protocol: http` |

- `{API_URL}` is `https://ai.gitee.com` in MoArk's official examples; the
  MoArk-branded base is `https://api.moark.com` (docs render a `{{API_URL}}`
  placeholder). Access tokens are created at **工作台 → 设置 → 访问令牌**.

## 3. MiniMax-H3 operations

Both interfaces share `path = v1/async/videos/generations`, `price = 0.5`
(billing unit `unit_tag 1264`), `status = available`. Each interface is
available on two compute clusters (distinct `operation` ids):

| operation | Interface | `task` value | vendor | region |
|-----------|-----------|--------------|--------|--------|
| `995` | 文生视频 / text-to-video | `t2va` | biren-tech | prod-br-sh-wz-ip |
| `996` | 首尾帧生视频 / first-last-frame | `fl2va` | biren-tech | prod-br-sh-wz-ip |
| `1005` | 文生视频 / text-to-video | `t2va` | metax | prod-mx-sh-ip |
| `1006` | 首尾帧生视频 / first-last-frame | `fl2va` | metax | prod-mx-sh-ip |

Playground (login required to run):
`https://moark.com/serverless-api?model=MiniMax-H3&operation=<995|996|1005|1006>`

### Request parameters

Every video request also carries `model: "MiniMax-H3"` and the `task` selector.

**文生视频 — `task = "t2va"`**

| Param | In | Type | Required | Default | Notes |
|-------|----|------|----------|---------|-------|
| `task` | body | string | yes | `t2va` | fixed select: 文生视频 |
| `duration_seconds` | body | integer | no | `6` | 生成视频时长, 4–15, step 1 |
| `num_steps` | body | integer | no | `20` | 推理步数, 5–50, step 1 |
| `aspect_ratio` | body | string | no | `16:9` | 视频比例: `auto` / `adaptive` / `9:16` / `1:1` / `4:3` / `3:4` / `16:9` |
| `seed` | body | integer | no | — | 随机过程可复现 |
| `X-Failover-Enabled` | head | boolean | no | `true` | 故障转移机制 |

*(Registry note: the `t2va` param list does not enumerate a `prompt` entry, but a
text prompt is the core input of text-to-video — pass `prompt` in the body. See §6.)*

**首尾帧生视频 — `task = "fl2va"`**

| Param | In | Type | Required | Default | Notes |
|-------|----|------|----------|---------|-------|
| `task` | body | string | yes | `fl2va` | fixed select: 首尾帧生视频 |
| `prompt` | body | string | yes | — | text prompt (sample: African savanna scene) |
| `first_frame` | body | string | yes | — | 首帧图片URL (e.g. `https://gitee-ai.su.bcebos.com/samples/images/first_frame.png`) |
| `last_frame` | body | string | no | — | 尾帧图片URL, 非必填 |
| `duration_seconds` | body | integer | no | `6` | 生成视频时长, 4–15 |
| `num_steps` | body | integer | no | `20` | 推理步数, 5–50 |
| `aspect_ratio` | body | string | no | `16:9` | `auto` / `adaptive` / `9:16` / `1:1` / `4:3` / `3:4` / `16:9` |
| `seed` | body | integer | no | — | 随机过程可复现 |
| `X-Failover-Enabled` | head | boolean | no | `true` | 故障转移机制 |

## 4. Async flow

All video models on MoArk are **async**: submit → get `task_id` → poll (or
webhook) for the result.

```bash
# 1) submit
curl -X POST "https://ai.gitee.com/v1/async/videos/generations" \
  -H "Authorization: Bearer $MOARK_TOKEN" \
  -H "Content-Type: application/json" \
  -H "X-Failover-Enabled: true" \
  -d '{
        "model": "MiniMax-H3",
        "task": "t2va",
        "prompt": "A scenic video of the African savanna at sunset",
        "duration_seconds": 6,
        "num_steps": 20,
        "aspect_ratio": "16:9"
      }'
# -> { "task_id": "1b0c1f9e6b0e4f62ab89f9f8e92f2c2a" }

# 2) poll
curl "https://ai.gitee.com/v1/task/<task_id>" \
  -H "Authorization: Bearer $MOARK_TOKEN"
```

```python
import requests, time

API_URL = "https://ai.gitee.com"
headers = {"Authorization": f"Bearer {TOKEN}"}

def submit(payload):
    return requests.post(f"{API_URL}/v1/async/videos/generations",
                         headers=headers, json=payload).json()

def poll(task_id, timeout=30*60, interval=10):
    url = f"{API_URL}/v1/task/{task_id}"
    for _ in range(timeout // interval):
        r = requests.get(url, headers=headers, timeout=10).json()
        if r.get("error"):
            raise ValueError(r["error"])
        if r.get("status") == "success":
            return r["output"]["file_url"]
        if r.get("status") in ("failed", "cancelled"):
            raise RuntimeError(r["status"])
        time.sleep(interval)
    raise TimeoutError(task_id)

task_id = submit({
    "model": "MiniMax-H3",
    "task": "t2va",
    "prompt": "A scenic video of the African savanna at sunset",
    "duration_seconds": 6,
    "num_steps": 20,
    "aspect_ratio": "16:9",
})["task_id"]
print(poll(task_id))
```

Task record / status values: `waiting`, `in_progress`, `success`, `failure`,
`cancelled`. On success the video URL is `output.file_url`; the record also has
`started_at` / `completed_at` (ms) and `usage_info`
(`unit`, `quantity`, `prompt_tokens`, `completion_tokens`, `resolution`).

## 5. Webhook callback

Callbacks are opt-in: the account's **WebHook 密钥** must be reset
(工作台 → 设置 → 个人信息), and the submit must carry `X-WebHook`.

Platform → your service: `POST` with `Content-Type: application/json`,
`X-Sign-Timestamp` (13-digit ms), `X-Signature` (lowercase hex HMAC-SHA256 of
`timestamp + "\n" + signedQueryString + "\n" + payloadText`, keyed by the WebHook
密钥). Return `2xx`.

```json
{
  "event_id": "7f2a278fad9c4300b4f114aaf30b84b6",
  "task_id": "1b0c1f9e6b0e4f62ab89f9f8e92f2c2a",
  "status": "success",
  "output": { "file_url": "https://example.com/output.mp4" },
  "usage_info": { "unit": "seconds", "quantity": 6,
                  "prompt_tokens": 0, "completion_tokens": 0, "resolution": null }
}
```

Query-string normalization for the signature: keep duplicates, sort by key then
value, blanks as `=`, join with `&` (e.g. `?b=2&a=3&a=1&empty` → `a=1&a=3&b=2&empty=`).

## 6. Caveats

- **`prompt` on `t2va`**: the registry param list for the 文生视频 operations
  (995 / 1005) omits `prompt`, yet text-to-video requires it and the `fl2va`
  operations list it explicitly. Send `prompt` for `t2va`; verify against the
  login-gated playground sample if a request is rejected.
- **Login-gated playground**: the per-model example code and the base URL used by
  the playground require sign-in; values here come from the public model registry
  and the open-source docs, and were not executed against the live API.
- **Two bases / duplicate ops**: `{API_URL}` = `https://ai.gitee.com` (official
  examples) vs `https://api.moark.com` (MoArk home). The same interface exists
  twice (biren-tech / metax) — the platform routes/fails over between them.

## 7. Links

- Video models docs: https://moark.com/docs/products/apis/videos/
- Async task guide: https://moark.com/docs/products/apis/async-task
- Serverless API overview: https://moark.com/docs/products/apis
- OpenAPI / interface reference: https://moark.com/docs/openapi/v1
- MiniMax-H3 playground: https://moark.com/serverless-api?model=MiniMax-H3
- Official examples repo: https://gitee.com/moark/examples/tree/master/videos
- Docs source: https://gitee.com/moark/docs