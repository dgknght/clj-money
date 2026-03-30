import { TEST_USER_EMAIL } from './constants.js';

const JSON_HEADERS = {
  'Content-Type': 'application/json',
  'Accept': 'application/json',
};

function authHeaders(authToken) {
  return { ...JSON_HEADERS, 'Authorization': `Bearer ${authToken}` };
}

async function apiPost(baseURL, path, body) {
  const res = await fetch(`${baseURL}${path}`, {
    method: 'POST',
    headers: JSON_HEADERS,
    body: JSON.stringify(body),
  });
  if (!res.ok) throw new Error(`POST ${path} failed: ${res.status}`);
  return res.json();
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

export default async function globalTeardown(config) {
  const adminEmail = process.env.E2E_ADMIN_EMAIL;
  const adminPassword = process.env.E2E_ADMIN_PASSWORD;
  if (!adminEmail || !adminPassword) return;

  const baseURL = config.projects[0].use.baseURL
    ?? process.env.BASE_URL
    ?? 'http://localhost:3000';

  const { authToken } = await apiPost(
    baseURL,
    '/oapi/users/authenticate',
    { email: adminEmail, password: adminPassword }
  );

  const users = await apiGet(baseURL, '/api/users', authToken);
  const testUser = users.find(u => u.email === TEST_USER_EMAIL);
  if (testUser) {
    await apiDelete(baseURL, `/api/users/${testUser.id}`, authToken);
  }
}
