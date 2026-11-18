import { test, expect } from '@playwright/test';

test('club links switch the local tenant and open its club page', async ({ page }) => {
  await page.goto('/tenants');
  const club = page.getByRole('link', { name: 'DSP Kometa Brno', exact: true });
  await expect(club).toHaveAttribute('href', '/api/auth/handoff?tenant=2&from=%2Ftanecni-klub');
  await club.click();
  await expect(page).toHaveURL(/\/tanecni-klub$/);
  await expect(page.locator('html')).toHaveAttribute('data-tenant', '2');
});

test('handoff preserves the return path through sign-in', async ({ playwright, baseURL }) => {
  const anonymous = await playwright.request.newContext({ baseURL, storageState: { cookies: [], origins: [] } });
  const response = await anonymous.get('/api/auth/handoff?tenant=2&from=%2Ftanecni-klub', { maxRedirects: 0 });
  const login = new URL(response.headers().location);
  expect(login.pathname).toBe('/login');
  expect(login.searchParams.get('from')).toBe('/api/auth/handoff?tenant=2&from=%2Ftanecni-klub');
  await anonymous.dispose();
});

test('handoff rejects unknown clubs and external return URLs', async ({ request }) => {
  const unknown = await request.get('/api/auth/handoff?tenant=999999', { maxRedirects: 0 });
  expect(new URL(unknown.headers().location).pathname).toBe('/otp/invalid');
  const external = await request.get('/api/auth/handoff?tenant=2&from=https://other.example', { maxRedirects: 0 });
  expect(new URL(external.headers().location).pathname).toBe('/dashboard');
});
