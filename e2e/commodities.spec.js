import { test, expect } from '@playwright/test';
import { TEST_USER_EMAIL, TEST_USER_PASSWORD } from './constants.js';

test.beforeEach(async ({ page }) => {
  if (!process.env.E2E_ADMIN_EMAIL || !process.env.E2E_ADMIN_PASSWORD) {
    test.skip(true, 'E2E_ADMIN_EMAIL and E2E_ADMIN_PASSWORD must be set');
  }
  await page.goto('/login');
  await page.locator('#email').fill(TEST_USER_EMAIL);
  await page.locator('#password').fill(TEST_USER_PASSWORD);
  await page.getByTitle('Click here to sign in.').click();
  await page.goto('/commodities');
});

test('add a commodity', async ({ page }) => {
  await page.getByTitle('Click here to add a new commodity.').click();
  await page.getByLabel('Type').selectOption('stock');
  await page.getByLabel('Exchange').selectOption('nyse');
  await page.getByLabel('Symbol').fill('AAPL');
  await page.getByLabel('Name').fill('Apple Inc.');
  await page.getByTitle('Click here to save this commodity').click();
  await expect(page.getByRole('cell', { name: 'Apple Inc.' })).toBeVisible();
});

test('edit a commodity', async ({ page }) => {
  // First add one to edit
  await page.getByTitle('Click here to add a new commodity.').click();
  await page.getByLabel('Type').selectOption('stock');
  await page.getByLabel('Exchange').selectOption('nyse');
  await page.getByLabel('Symbol').fill('GOOG');
  await page.getByLabel('Name').fill('Alphabet Inc.');
  await page.getByTitle('Click here to save this commodity').click();
  await expect(page.getByRole('cell', { name: 'Alphabet Inc.' })).toBeVisible();

  // Now edit it
  await page.getByRole('row', { name: /Alphabet Inc\./ })
    .getByTitle('Click here to edit this commodity.')
    .click();
  await page.getByLabel('Name').fill('Alphabet Inc. (Google)');
  await page.getByTitle('Click here to save this commodity').click();
  await expect(
    page.getByRole('cell', { name: 'Alphabet Inc. (Google)' })
  ).toBeVisible();
});

test('cancel editing a commodity', async ({ page }) => {
  // Add a commodity first
  await page.getByTitle('Click here to add a new commodity.').click();
  await page.getByLabel('Type').selectOption('stock');
  await page.getByLabel('Exchange').selectOption('nasdaq');
  await page.getByLabel('Symbol').fill('MSFT');
  await page.getByLabel('Name').fill('Microsoft Corp.');
  await page.getByTitle('Click here to save this commodity').click();
  await expect(page.getByRole('cell', { name: 'Microsoft Corp.' })).toBeVisible();

  // Open edit, change name, then cancel
  await page.getByRole('row', { name: /Microsoft Corp\./ })
    .getByTitle('Click here to edit this commodity.')
    .click();
  await page.getByLabel('Name').fill('Changed Name');
  await page.getByTitle('Click here to discontinue this edit operation.').click();
  await expect(page.getByRole('cell', { name: 'Microsoft Corp.' })).toBeVisible();
  await expect(page.getByRole('cell', { name: 'Changed Name' })).not.toBeVisible();
});

test('delete a commodity', async ({ page }) => {
  // Add a commodity to delete
  await page.getByTitle('Click here to add a new commodity.').click();
  await page.getByLabel('Type').selectOption('stock');
  await page.getByLabel('Exchange').selectOption('nyse');
  await page.getByLabel('Symbol').fill('IBM');
  await page.getByLabel('Name').fill('IBM Corp.');
  await page.getByTitle('Click here to save this commodity').click();
  await expect(page.getByRole('cell', { name: 'IBM Corp.' })).toBeVisible();

  // Delete it
  page.once('dialog', dialog => dialog.accept());
  await page.getByRole('row', { name: /IBM Corp\./ })
    .getByTitle('Click here to delete this commodity.')
    .click();
  await expect(page.getByRole('cell', { name: 'IBM Corp.' })).not.toBeVisible();
});
