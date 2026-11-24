import { defineConfig, devices } from '@playwright/test';

const port = 5100;
const externalURL = process.env.PLAYWRIGHT_BASE_URL;
const baseURL = externalURL || `http://localhost:${port}`;

export default defineConfig({
  testDir: './tests',
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  reporter: 'html',
  webServer: externalURL ? undefined : {
    command: `pnpm --filter @rozpisovnik/web exec next dev --hostname localhost --port ${port}`,
    env: { GRAPHILE_ENV: 'development' },
    port,
    reuseExistingServer: !process.env.CI,
  },
  use: {
    baseURL,
    trace: 'on-first-retry',
  },
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
  ],
});
