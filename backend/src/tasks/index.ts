import { forgottenPasswordGenerate } from './forgottenPasswordGenerate';
import { notifyAdminRegistration } from './notifyAdminRegistration';
import { sendEmail } from './sendEmail'

export default {
  send_email: sendEmail,
  notify_admin_registration: notifyAdminRegistration,
  forgotten_password_generate: forgottenPasswordGenerate,
};
