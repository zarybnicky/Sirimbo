import { forgottenPasswordGenerate } from './forgottenPasswordGenerate';
import { notifyAdminRegistration } from './notifyAdminRegistration';
import { notifyConfirmedUser } from './notifyConfirmedUser';
import { sendEmail } from './sendEmail'
import { sendInvitation } from './sendInvitation'

export default {
  send_email: sendEmail,
  send_invitation: sendInvitation,
  notify_admin_registration: notifyAdminRegistration,
  forgotten_password_generate: forgottenPasswordGenerate,
  notify_confirmed_user: notifyConfirmedUser,
};
