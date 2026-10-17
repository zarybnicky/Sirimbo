import type { Task } from 'graphile-worker';

export const task: Task<'send_invitation'> = async ({ id }, workerUtils) => {
  const {
    rows: [invitation],
  } = await workerUtils.withPgClient((pgClient) =>
    pgClient.query<{
      access_token: string;
      tenant_name: string;
      tenant_domain: string;
      target_email: string;
      instance_id: string | null;
      event_name: string | null;
    }>(
      `select
  person_invitation.access_token as access_token,
  tenant.name as tenant_name,
  tenant.origins[1] as tenant_domain,
  person_invitation.email as target_email,
  person_invitation.instance_id,
  event_instance.name as event_name
from person_invitation
inner join tenant on person_invitation.tenant_id=tenant.id
left join event_instance on event_instance.id=person_invitation.instance_id
where person_invitation.id = $1`,
      [id],
    ),
  );
  if (!invitation) {
    console.error('Invitation not found; aborting');
    return;
  }
  await workerUtils.addJob('send_email', {
    template: 'send_invitation.mjml',
    options: {
      to: invitation.target_email,
      subject: invitation.instance_id
        ? `[Rozpisovnik] Potvrzení přihlášky: ${invitation.event_name || 'Akce'}`
        : '[Rozpisovnik] Pozvánka do systému',
    },
    variables: {
      tenant: invitation.tenant_name,
      invitationLink: `${invitation.tenant_domain}/pozvanka?token=${invitation.access_token}${invitation.instance_id ? `&from=${encodeURIComponent(`/termin/${invitation.instance_id}`)}` : ''}`,
      eventName: invitation.instance_id ? invitation.event_name || 'Akce' : null,
      homeLink: invitation.instance_id
        ? `${invitation.tenant_domain}/termin/${invitation.instance_id}`
        : invitation.tenant_domain,
      domain: invitation.tenant_domain.replace('http://', '').replace('https://', ''),
    },
  });
};

export default task;
