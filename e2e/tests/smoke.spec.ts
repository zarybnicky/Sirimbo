import { test, expect } from '@playwright/test';

test('event form closes and can be opened again', async ({ page }) => {
  await page.goto('/akce');
  await page.getByRole('button', { name: 'PŘIDAT UDÁLOST' }).click();
  await expect(page.getByRole('dialog').locator('form')).toBeVisible();
  await page
    .getByRole('dialog')
    .getByRole('button', { name: 'Zavřít', exact: true })
    .click();
  await expect(page.getByRole('dialog')).toHaveCount(0);
  await expect
    .poll(() => page.evaluate(() => getComputedStyle(document.body).pointerEvents))
    .not.toBe('none');
  await page.getByRole('button', { name: 'PŘIDAT UDÁLOST' }).click();
  await expect(page.getByRole('dialog').locator('form')).toBeVisible();
});
