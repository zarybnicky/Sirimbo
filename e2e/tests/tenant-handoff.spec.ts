import { test, expect } from '@playwright/test';

test('signed-in handoff switches the local tenant and opens its club page', async ({ page }) => {
  const from = '/api/auth/handoff?tenant=2&from=%2Ftanecni-klub';
  await page.goto(`/login/elevated?${new URLSearchParams({ role: 'administrator', from })}`);
  await expect(page).toHaveURL(/\/tanecni-klub$/);
  await expect(page.locator('html')).toHaveAttribute('data-tenant', '2');
});

test('handoff preserves the return path through sign-in', async ({ request }) => {
  const response = await request.get('/api/auth/handoff?tenant=2&from=%2Ftanecni-klub', { maxRedirects: 0 });
  const login = new URL(response.headers().location);
  expect(login.pathname).toBe('/login');
  expect(login.searchParams.get('from')).toBe('/api/auth/handoff?tenant=2&from=%2Ftanecni-klub');
});

test('handoff rejects unknown clubs and external return URLs', async ({ request }) => {
  const unknown = await request.get('/api/auth/handoff?tenant=999999', { maxRedirects: 0 });
  expect(new URL(unknown.headers().location).pathname).toBe('/otp/invalid');
  const external = await request.get('/api/auth/handoff?tenant=2&from=https://other.example', { maxRedirects: 0 });
  expect(new URL(external.headers().location).pathname).toBe('/dashboard');
});
