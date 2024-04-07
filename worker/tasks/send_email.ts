import { Task } from 'graphile-worker';
import mjml2html from "mjml";
import { htmlToText } from "html-to-text";
import * as nodemailer from "nodemailer";
import SMTPTransport from 'nodemailer/lib/smtp-transport';
import { promises } from 'fs';
import Handlebars from "handlebars";

const fromEmail = "Rozpisovník.cz <info@rozpisovnik.cz>";

const task: Task<"send_email"> = async (payload) => {
  const { options, template, variables } = payload;

  const transport = await getTransport();
  if (template) {
    if (!template.match(/^[a-zA-Z0-9_.-]+$/)) {
      throw new Error(`Disallowed template name '${template}'`);
    }
    const templateSrc = await promises.readFile(`${__dirname}/../templates/${template}`, "utf8");
    const mjmlResult = mjml2html(templateSrc, {
      preprocessors: [
        (src) => Handlebars.compile(src)(variables)
      ],
    });
    if (mjmlResult.errors?.length > 0) {
      console.error(mjmlResult.errors);
    }
    options.html = mjmlResult.html;
    options.text = htmlToText(options.html, { wordwrap: 120 }).replace(/\n\s+\n/g, "\n\n");
  }
  await transport.sendMail({
    from: fromEmail,
    ...options,
  });
}

let transporterPromise: Promise<nodemailer.Transporter>;
function getTransport(): Promise<nodemailer.Transporter> {
  if (!transporterPromise) {
    transporterPromise = (async () => {
      const options: SMTPTransport.Options = {
        host: process.env.SMTP_HOST,
        port: parseInt(process.env.SMTP_PORT || '25', 10),
        secure: !!process.env.SMTP_TLS,
        connectionTimeout: 60000,
        greetingTimeout: 30000,
        debug: true,
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

export default task;
