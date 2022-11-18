import { makeWorkerUtils } from 'graphile-worker';

async function main() {
  const workerUtils = await makeWorkerUtils({ connectionString: process.env.GM_DBURL });
  try {
    await workerUtils.migrate();
  } finally {
    await workerUtils.release();
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
