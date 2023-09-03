import { forgottenPasswordGenerate } from './forgottenPasswordGenerate';
import { notifyConfirmedUser } from './notifyConfirmedUser';
import { sendEmail } from './sendEmail'
import { sendInvitation } from './sendInvitation'

export default {
  send_email: sendEmail,
  send_invitation: sendInvitation,
  forgotten_password_generate: forgottenPasswordGenerate,
  notify_confirmed_user: notifyConfirmedUser,
};
