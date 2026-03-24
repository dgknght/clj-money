import { test, expect } from '@playwright/test';

const email = process.env.E2E_EMAIL;
const password = process.env.E2E_PASSWORD;

test.beforeEach(() => {
  if (!email || !password) {
    test.skip(true, 'E2E_EMAIL and E2E_PASSWORD must be set');
  }
});

test('login with email and password', async ({ page }) => {
  await page.goto('/login');
  await page.locator('#email').fill(email);
  await page.locator('#password').fill(password);
  await page.getByTitle('Click here to sign in.').click();
  await expect(
    page.getByTitle('Click here to sign out of the system')
  ).toBeVisible();
});

test('logout', async ({ page }) => {
  await page.goto('/login');
  await page.locator('#email').fill(email);
  await page.locator('#password').fill(password);
  await page.getByTitle('Click here to sign in.').click();
  await page.getByTitle('Click here to sign out of the system').click();
  await expect(page.locator('#login')).toBeVisible();
});
