import { chromium, FullConfig } from '@playwright/test';
import * as fs from 'fs';
import * as path from 'path';

async function globalSetup(config: FullConfig) {
  const baseURL = config.projects[0].use.baseURL ?? 'http://localhost:3000';
  const authFile = path.join(__dirname, '.auth/user.json');
  fs.mkdirSync(path.dirname(authFile), { recursive: true });

  const browser = await chromium.launch();
  const page = await browser.newPage();

  // Navigate to login page
  // Note: any auth-required page also shows the login form via Layout,
  // but /login is the explicit route.
  await page.goto(baseURL + '/login');

  // Field names from frontend/ui/forms/LoginForm.tsx:
  // name="login" (email or username), name="passwd" (password)
  await page.fill('[name="login"]', process.env.E2E_EMAIL ?? 'admin@test.cz');
  await page.fill('[name="passwd"]', process.env.E2E_PASSWORD ?? 'admin');
  await page.click('[type="submit"]');

  // Wait for redirect away from the login page
  await page.waitForURL((url) => !url.pathname.startsWith('/login'));

  await page.context().storageState({ path: authFile });
  await browser.close();
}

export default globalSetup;
