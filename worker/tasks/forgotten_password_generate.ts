import type { Task } from 'graphile-worker';

const task: Task<'forgotten_password_generate'> = async (payload, workerUtils) => {
  const { origin, users } = payload;
  const user = users.find((x) => x);

  if (!user) return;

  await workerUtils.addJob('send_email', {
    template: 'forgotten_password_generate.mjml',
    options: {
      to: user.email,
      subject: '[Rozpisovnik] Zapomenuté heslo',
    },
    variables: {
      origin: origin + '/otp?token=',
      users,
    },
  });
};

export default task;
