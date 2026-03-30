import { test, expect } from '@playwright/test';
import { TEST_USER_EMAIL, TEST_USER_PASSWORD } from './constants.js';

test.beforeEach(() => {
  if (!process.env.E2E_ADMIN_EMAIL || !process.env.E2E_ADMIN_PASSWORD) {
    test.skip(true, 'E2E_ADMIN_EMAIL and E2E_ADMIN_PASSWORD must be set');
  }
});

test('login with email and password', async ({ page }) => {
  await page.goto('/login');
  await page.locator('#email').fill(TEST_USER_EMAIL);
  await page.locator('#password').fill(TEST_USER_PASSWORD);
  await page.getByTitle('Click here to sign in.').click();
  await expect(
    page.getByTitle('Click here to sign out of the system')
  ).toBeVisible();
});

test('logout', async ({ page }) => {
  await page.goto('/login');
  await page.locator('#email').fill(TEST_USER_EMAIL);
  await page.locator('#password').fill(TEST_USER_PASSWORD);
  await page.getByTitle('Click here to sign in.').click();
  await page.getByTitle('Click here to sign out of the system').click();
  await expect(page.locator('#login')).toBeVisible();
});
