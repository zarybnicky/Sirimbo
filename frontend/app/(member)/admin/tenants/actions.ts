'use server';

import { runQuery } from '@/lib/server/postgresql';
import { getRequestContext } from '@/lib/server/tenant';
import { enqueueJob } from '@/lib/jobs.queries';

export async function sendTestEmail() {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) throw new Error('FORBIDDEN');

  await runQuery(enqueueJob, {
    identifier: 'send_email',
    payload: JSON.stringify({
      options: {
        to: auth.email?.trim(),
        subject: '[Rozpisovník] Test e-mail',
        text: [
          `Sent: ${new Date().toISOString()}`,
          `User: ${auth.userId}`,
        ].join('\n'),
      },
    }),
  });
}
