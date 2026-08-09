import { test, expect } from '@playwright/test';

test('event form opens', async ({ page }) => {
  await page.goto('/akce');
  await page.getByRole('button', { name: 'PŘIDAT UDÁLOST' }).click();
  await expect(page.getByRole('dialog').locator('form')).toBeVisible();
});
