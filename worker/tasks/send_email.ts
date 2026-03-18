import { promises } from 'fs';
import type { Task } from 'graphile-worker';
import Handlebars from "handlebars";
import { htmlToText } from "html-to-text";
import mjml2html from "mjml";
import { createTransport, type Transporter } from "nodemailer";

const fromEmail = "Rozpisovník.cz <info@rozpisovnik.cz>";

const task: Task<"send_email"> = async (payload) => {
  const { options, template, variables } = payload;

  await new Promise(r => setTimeout(r, 250));

  const transport = await getTransport();
  if (template) {
    if (!template.match(/^[a-zA-Z0-9_.-]+$/)) {
      throw new Error(`Disallowed template name '${template}'`);
    }
    const templateSrc = await promises.readFile(`${import.meta.dirname}/../templates/${template}`, "utf8");
    const mjmlResult = mjml2html(templateSrc, {
      preprocessors: [
        (src) => Handlebars.compile(src)(variables)
      ],
    });
    if (mjmlResult.errors?.length > 0) {
      console.error(mjmlResult.errors);
    }
    options.html = mjmlResult.html;
    options.text = htmlToText(options.html, { wordwrap: 120 }).replaceAll(/\n\s+\n/g, "\n\n");
  }
  await transport.sendMail({
    from: fromEmail,
    ...options,
  });
}

let transporterPromise: Promise<Transporter>;
function getTransport(): Promise<Transporter> {
  if (!transporterPromise) {
    transporterPromise = (async () => createTransport({
      host: process.env.SMTP_HOST,
      port: Number.parseInt(process.env.SMTP_PORT || '25', 10),
      secure: !!process.env.SMTP_TLS,
      connectionTimeout: 60000,
      greetingTimeout: 30000,
      debug: true,
      auth: process.env.SMTP_AUTH ? {
        user: process.env.SMTP_USER,
        pass: process.env.SMTP_PASS,
      } : undefined,
    }))();
  }
  return transporterPromise;
}

export default task;
