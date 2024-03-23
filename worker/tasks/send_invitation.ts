import { Task } from 'graphile-worker';

export const task: Task<"send_invitation"> = async ({ id }, workerUtils) => {
  const { rows: [invitation] } = await workerUtils.withPgClient((pgClient) =>
    pgClient.query<{
      access_token: string;
      tenant_name: string;
      tenant_domain: string;
      target_email: string;
    }>(`select
  person_invitation.access_token as access_token,
  tenant.name as tenant_name,
  tenant.origins[1] as tenant_domain,
  person_invitation.email as target_email
from person_invitation
inner join tenant on person_invitation.tenant_id=tenant.id
where person_invitation.id = $1`, [id])
  );
  if (!invitation) {
    console.error("Invitation not found; aborting");
    return;
  }
  await workerUtils.addJob("send_email", {
    template: "send_invitation.mjml",
    options: {
      to: invitation.target_email,
      subject: "[Rozpisovnik] Pozvánka do systému",
    },
    variables: {
      tenant: invitation.tenant_name,
      link: `${invitation.tenant_domain}/pozvanka?token=${invitation.access_token}`,
    },
  });
};

export default task;
