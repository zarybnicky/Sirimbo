import { forgottenPasswordGenerate } from './forgottenPasswordGenerate';
import { sendEmail } from './sendEmail'
import { sendInvitation } from './sendInvitation'

export default {
  send_email: sendEmail,
  send_invitation: sendInvitation,
  forgotten_password_generate: forgottenPasswordGenerate,
};
