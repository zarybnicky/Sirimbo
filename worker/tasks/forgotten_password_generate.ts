import { Task } from 'graphile-worker';

const task: Task<"forgotten_password_generate"> = async (payload, workerUtils) => {
  let { origin, intent, users } = payload;
  const user = users.find(x => x);

  if (!user) return;

  await workerUtils.addJob("send_email", {
    template: "forgotten_password_generate.mjml",
    options: {
      to: user.email,
      subject: "[Rozpisovnik] Zapomenuté heslo",
    },
    variables: {
      origin: origin + `/otp?intent=${intent}&token=`,
      users,
    },
  });
};

export default task;
