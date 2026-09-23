'use server';

import { withTransaction } from '@/lib/server/postgresql';
import { getRequestContext } from '@/lib/server/tenant';

export async function sendTestEmail() {
  const { claims } = await getRequestContext();
  if (!claims || claims.role !== 'system_admin') throw new Error('FORBIDDEN');

  await withTransaction(async (client) => {
    await client.query('select graphile_worker.add_job($1, $2::json)', [
      'send_email',
      JSON.stringify({
        options: {
          to: claims.email.trim(),
          subject: '[Rozpisovník] Test e-mail',
          text: [
            `Sent: ${new Date().toISOString()}`,
            `User: ${claims.user_id}`,
          ].join('\n'),
        },
      }),
    ]);
  });
}
