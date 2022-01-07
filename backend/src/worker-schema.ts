import { makeWorkerUtils } from 'graphile-worker';

async function main() {
  const workerUtils = await makeWorkerUtils({});
  try {
    await workerUtils.migrate();
    // await workerUtils.addJob("send_email", {
    //   template: "test_email.mjml",
    //   options: {
    //     to: "jakub@zarybnicky.com",
    //     subject: "Test email",
    //   }
    // });
  } finally {
    await workerUtils.release();
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
