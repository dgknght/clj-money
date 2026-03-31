import {
  TEST_USER_EMAIL,
  TEST_USER_PASSWORD,
  TEST_USER_FIRST_NAME,
  TEST_USER_LAST_NAME,
  TEST_ENTITY_NAME,
} from './constants.js';

const JSON_HEADERS = {
  'Content-Type': 'application/json',
  'Accept': 'application/json',
};

function authHeaders(authToken) {
  return { ...JSON_HEADERS, 'Authorization': `Bearer ${authToken}` };
}

async function apiPost(baseURL, path, body, authToken) {
  const res = await fetch(`${baseURL}${path}`, {
    method: 'POST',
    headers: authToken ? authHeaders(authToken) : JSON_HEADERS,
    body: JSON.stringify(body),
  });
  if (!res.ok && res.status !== 400) {
    throw new Error(`POST ${path} failed: ${res.status} ${await res.text()}`);
  }
  const text = await res.text();
  return text ? JSON.parse(text) : null;
}

async function apiGet(baseURL, path, authToken) {
  const res = await fetch(`${baseURL}${path}`, {
    headers: authHeaders(authToken),
  });
  if (!res.ok) throw new Error(`GET ${path} failed: ${res.status}`);
  return res.json();
}

async function apiDelete(baseURL, path, authToken) {
  const res = await fetch(`${baseURL}${path}`, {
    method: 'DELETE',
    headers: authHeaders(authToken),
  });
  if (!res.ok && res.status !== 404) {
    throw new Error(`DELETE ${path} failed: ${res.status}`);
  }
}

export default async function globalSetup(config) {
  const adminEmail = process.env.E2E_ADMIN_EMAIL;
  const adminPassword = process.env.E2E_ADMIN_PASSWORD;
  if (!adminEmail || !adminPassword) {
    console.warn(
      'E2E_ADMIN_EMAIL and E2E_ADMIN_PASSWORD must be set to run e2e tests'
    );
    return;
  }

  const baseURL = config.projects[0].use.baseURL
    ?? process.env.BASE_URL
    ?? 'http://localhost:3000';

  const { authToken } = await apiPost(
    baseURL,
    '/oapi/users/authenticate',
    { email: adminEmail, password: adminPassword }
  );

  // Delete the test user from any previous run
  const users = await apiGet(baseURL, '/api/users', authToken);
  const existing = users.find(u => u.email === TEST_USER_EMAIL);
  if (existing) {
    await apiDelete(baseURL, `/api/users/${existing.id}`, authToken);
  }

  // Create an invitation for the test user
  const invitation = await apiPost(
    baseURL,
    '/api/invitations',
    { _type: 'invitation', recipient: TEST_USER_EMAIL },
    authToken
  );

  // Accept the invitation to create the test user
  const { authToken: testUserToken } = await apiPost(
    baseURL,
    `/oapi/invitations/${invitation.token}/accept`,
    {
      _type: 'user',
      firstName: TEST_USER_FIRST_NAME,
      lastName: TEST_USER_LAST_NAME,
      password: TEST_USER_PASSWORD,
    }
  );

  // Create a test entity for the test user
  await apiPost(
    baseURL,
    '/api/entities',
    { _type: 'entity', name: TEST_ENTITY_NAME },
    testUserToken
  );
}
