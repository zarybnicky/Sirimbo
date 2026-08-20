'use server';

import { withRequestPgClient } from '@/lib/server/postgresql';

export async function sendTestEmail() {
  await withRequestPgClient(async (client, settings) => {
    if (settings.role !== 'system_admin') throw new Error('FORBIDDEN');

    await client.query('select graphile_worker.add_job($1, $2::json)', [
      'send_email',
      JSON.stringify({
        options: {
          to: settings['jwt.claims.email']?.trim(),
          subject: '[Rozpisovník] Test e-mail',
          text: [
            `Sent: ${new Date().toISOString()}`,
            `User: ${settings['jwt.claims.user_id'] || 'unknown'}`,
          ].join('\n'),
        },
      }),
    ]);
  });
}
