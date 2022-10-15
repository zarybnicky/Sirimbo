import { forgottenPasswordGenerate } from './forgottenPasswordGenerate';
import { notifyAdminRegistration } from './notifyAdminRegistration';
import { notifyConfirmedUser } from './notifyConfirmedUser';
import { sendEmail } from './sendEmail'

export default {
  send_email: sendEmail,
  notify_admin_registration: notifyAdminRegistration,
  forgotten_password_generate: forgottenPasswordGenerate,
  notify_confirmed_user: notifyConfirmedUser,
};
