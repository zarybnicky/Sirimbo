import { test, expect } from '@playwright/test';

test('calendar page loads', async ({ page }) => {
  await page.goto('/');
  await expect(page).toHaveTitle(/Rozpisovník/);
});

test('calendar shows main content', async ({ page }) => {
  await page.goto('/');
  await expect(page.locator('main')).toBeVisible();
});

test('event form opens', async ({ page }) => {
  await page.goto('/akce/nova');
  await expect(page.locator('form')).toBeVisible();
});
