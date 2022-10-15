import { Task } from 'graphile-worker';
import { SendEmailPayload } from './sendEmail';

type Payload = {
  id: string;
}

export const notifyConfirmedUser: Task = async (payload, workerUtils) => {
  const { id } = payload as Payload;
  const { rows: [user] } = await workerUtils.withPgClient((pgClient) =>
    pgClient.query('select users.* from users where u_id = $1', [id]),
  );
  if (!user) {
    console.error("User not found; aborting");
    return;
  }
  await workerUtils.addJob("send_email", {
    template: "notify_confirmed_user.mjml",
    options: {
      to: "jakub@zarybnicky.com",
      subject: "[Olymp] Váš účet byl potvrzen",
    },
    variables: {
      login: user.u_login,
    },
  } as SendEmailPayload);
};
