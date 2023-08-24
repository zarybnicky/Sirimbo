import { Task } from 'graphile-worker';
import lodashTemplate from "lodash.template";
import mjml2html from "mjml";
import { htmlToText } from "html-to-text";
import * as nodemailer from "nodemailer";
import SMTPTransport from 'nodemailer/lib/smtp-transport';
import { promises } from 'fs';

const fromEmail = "TK Olymp.cz <root@tkolymp.cz>";

// await workerUtils.addJob("send_email", {
//   template: "test_email.mjml",
//   options: {
//     to: "jakub@zarybnicky.com",
//     subject: "Test email",
//   }
// } as SendEmailPayload);


export type SendEmailPayload = {
  options: {
    from?: string;
    to: string | string[];
    subject: string;
    html?: string;
    text?: string;
  };
  template: string;
  variables: {
    [varName: string]: any;
  };
}

export const sendEmail: Task = async (payload) => {
  const { options, template, variables } = payload as SendEmailPayload;
  const transport = await getTransport();
  if (template) {
    options.html = (await loadTemplate(template))(variables);
    options.text = htmlToText(options.html, { wordwrap: 120 }).replace(/\n\s+\n/g, "\n\n");
  },
  await transport.sendMail({
    from: fromEmail,
    ...options,
  });
}

async function loadTemplate(template: string) {
  if (!template.match(/^[a-zA-Z0-9_.-]+$/)) {
    throw new Error(`Disallowed template name '${template}'`);
  }
  const templateFn = lodashTemplate(
    await promises.readFile(`${__dirname}/templates/${template}`, "utf8"),
    { escape: /\[\[([\s\S]+?)\]\]/g }
  );
  return (variables: { [varName: string]: any }) => {
    const { html, errors } = mjml2html(templateFn(variables));
    if (errors && errors.length) {
      console.error(errors);
    }
    return html;
  };
}

let transporterPromise: Promise<nodemailer.Transporter>;
export function getTransport(): Promise<nodemailer.Transporter> {
  if (!transporterPromise) {
    transporterPromise = (async () => {
      const options: SMTPTransport.Options = {
        host: process.env.SMTP_HOST,
        port: parseInt(process.env.SMTP_PORT || '25', 10),
        secure: !!process.env.SMTP_TLS,
      };
      if (process.env.SMTP_AUTH) {
        options.auth = {
          user: process.env.SMTP_USER,
          pass: process.env.SMTP_PASS,
        };
      }
      return nodemailer.createTransport(options);
    })();
  }
  return transporterPromise!;
}
