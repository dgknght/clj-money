# CLI Authentication - Client Implementation Guide

This document describes how to implement the device authorization flow
in a CLI client to authenticate with the clj-money service.

## Overview

The authentication flow follows the OAuth 2.0 Device Authorization Grant
(RFC 8628), which allows input-constrained devices (like CLI applications)
to obtain user authorization through a secondary device with better input
capabilities (like a web browser).

## Authentication Flow

### Step 1: Initiate Device Authorization

Make a POST request to initiate the device authorization flow:

**Endpoint:** `POST /oapi/cli/auth/device`

**Headers:**
- `Content-Type: application/json`

**Request Body:** None required (empty JSON object `{}` is acceptable)

**Response (200 OK):**
```json
{
  "deviceCode": "550e8400-e29b-41d4-a716-446655440000",
  "userCode": "ABCD3FGH",
  "verificationUri": "https://app.example.com/cli/authorize",
  "verificationUriComplete": "https://app.example.com/cli/authorize?user_code=ABCD3FGH",
  "expiresIn": 900,
  "interval": 5
}
```

**Response Fields:**
- `deviceCode`: High-entropy code for polling (keep this secret)
- `userCode`: 8-character code for the user to enter (display to user)
- `verificationUri`: URL where the user should authenticate
- `verificationUriComplete`: Complete URL with user code pre-filled
- `expiresIn`: Number of seconds until the codes expire (900 = 15 minutes)
- `interval`: Minimum number of seconds between polling requests (5 seconds)

### Step 2: Display Instructions to User

Show the user the `userCode` and `verificationUri`. For example:

```
To authenticate, please visit: https://app.example.com/cli/authorize
Enter this code: ABCD3FGH
Press any key when you have completed authorization...
```

Alternatively, you can open the `verificationUriComplete` URL in the user's
default browser automatically.

### Step 3: Poll for Access Token

While the user is authorizing, poll the token endpoint at the specified
`interval` (minimum 5 seconds between requests).

**Endpoint:** `POST /oapi/cli/auth/token`

**Headers:**
- `Content-Type: application/json`

**Request Body:**
```json
{
  "deviceCode": "550e8400-e29b-41d4-a716-446655440000"
}
```

**Successful Response (200 OK):**
```json
{
  "accessToken": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "tokenType": "Bearer",
  "expiresIn": 900
}
```

Store the `accessToken` securely for future API requests.

**Error Responses:**

1. **Authorization Pending (400 Bad Request):**
```json
{
  "error": "authorization_pending",
  "errorDescription": "The user has not yet completed authorization"
}
```
Continue polling after waiting the `interval` duration.

2. **Slow Down (400 Bad Request):**
```json
{
  "error": "slow_down",
  "errorDescription": "Polling too frequently"
}
```
Increase your polling interval by 5 seconds and continue.

3. **Access Denied (403 Forbidden):**
```json
{
  "error": "access_denied",
  "errorDescription": "The user denied the authorization request"
}
```
Stop polling. The user explicitly denied authorization.

4. **Expired Token (400 Bad Request):**
```json
{
  "error": "expired_token",
  "errorDescription": "The device code has expired"
}
```
Stop polling. The codes have expired (after 15 minutes). Start over from Step 1.

5. **Invalid Request (400 Bad Request):**
```json
{
  "error": "invalid_request",
  "errorDescription": "Invalid device_code"
}
```
Stop polling. The device code is invalid.

### Step 4: Use the Access Token

Once you receive the access token, use it in subsequent API requests:

**Header:**
```
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

## Example Implementation (Pseudocode)

```python
import requests
import time

# Step 1: Initiate device flow
response = requests.post(
    'https://api.example.com/oapi/cli/auth/device',
    headers={'Content-Type': 'application/json'},
    json={}
)
auth_data = response.json()

device_code = auth_data['deviceCode']
user_code = auth_data['userCode']
verification_uri = auth_data['verificationUri']
interval = auth_data['interval']
expires_in = auth_data['expiresIn']

# Step 2: Display instructions
print(f"Please visit: {verification_uri}")
print(f"Enter code: {user_code}")
print("Waiting for authorization...")

# Step 3: Poll for token
start_time = time.time()
while (time.time() - start_time) < expires_in:
    time.sleep(interval)

    response = requests.post(
        'https://api.example.com/oapi/cli/auth/token',
        headers={'Content-Type': 'application/json'},
        json={'deviceCode': device_code}
    )

    if response.status_code == 200:
        # Success!
        token_data = response.json()
        access_token = token_data['accessToken']
        print("Authentication successful!")
        # Store access_token securely
        break

    error_data = response.json()
    error = error_data.get('error')

    if error == 'authorization_pending':
        # Keep polling
        continue
    elif error == 'slow_down':
        # Increase interval
        interval += 5
        continue
    elif error in ['access_denied', 'expired_token', 'invalid_request']:
        # Stop polling
        print(f"Authentication failed: {error}")
        break

# Step 4: Use the token
headers = {
    'Authorization': f'Bearer {access_token}',
    'Content-Type': 'application/json'
}
response = requests.get(
    'https://api.example.com/api/users/me',
    headers=headers
)
user_data = response.json()
```

## Security Considerations

1. **Device Code Storage**: The `deviceCode` should be kept secret and
   never displayed to the user. It acts as a credential during the polling
   phase.

2. **User Code Display**: The `userCode` is meant to be displayed to the
   user and is designed to be easy to type (8 characters, excluding
   confusing characters like 0, O, 1, I, l).

3. **Token Storage**: Store the access token securely (e.g., in a keychain
   or secure credential store). Never log or display the token.

4. **HTTPS**: Always use HTTPS for all API requests to protect the tokens
   in transit.

5. **Token Expiration**: The access token may expire. Implement token
   refresh or re-authentication as needed.

6. **Polling Behavior**: Respect the `interval` value to avoid overwhelming
   the server. Implement exponential backoff on connection failures.

## Troubleshooting

**Problem**: "Invalid device_code" error immediately after initiating flow
- **Solution**: Ensure you're using the exact `deviceCode` from the
  initiation response. The code is case-sensitive.

**Problem**: "Expired token" error before user completes authorization
- **Solution**: The codes expire after 15 minutes. The user needs to
  complete authorization faster, or restart the flow.

**Problem**: Polling returns errors or times out
- **Solution**: Check your network connection. Ensure the server is
  reachable. Implement retry logic with exponential backoff.

**Problem**: User says they authorized but polling still returns "authorization_pending"
- **Solution**: Verify the user entered the correct `userCode`. The codes
  are case-sensitive. Check if the user is logged into the correct account.
