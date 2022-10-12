import { Task } from 'graphile-worker';
import { SendEmailPayload } from './sendEmail';

type Payload = {
  id: string;
}

export const forgottenPasswordGenerate: Task = async (payload, workerUtils) => {
  const { id } = payload as Payload;
  const { rows: [user] } = await workerUtils.withPgClient((pgClient) =>
    pgClient.query('select users.* from users where u_id = $1', [id]),
  );
  if (!user) {
    console.error("User not found; aborting");
    return;
  }

  const newPassword = [...Array(12)].map(() => Math.random().toString(36)[2]).join('');
  await workerUtils.withPgClient((pgClient) =>
    pgClient.query('update users set u_pass=$2 where u_id=$1', [id, newPassword]),
  );

  await workerUtils.addJob("send_email", {
    template: "forgotten_password_generate.mjml",
    options: {
      to: user.u_email,
      subject: "[Olymp] Zapomenuté heslo",
    },
    variables: {
      newPassword,
    },
  } as SendEmailPayload);
};
