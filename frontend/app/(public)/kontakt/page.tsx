/* eslint-disable import-x/no-unused-modules */
import { publicPageMetadata } from '@/lib/server/seo';
import { Mail, Phone } from 'lucide-react';
import { PageHeader } from '@/ui/TitleBar';
import { cardCls } from '@/ui/style';

export const generateMetadata = () => publicPageMetadata({
  title: 'Kontakt',
  description:
      'Kontakty na TK Olymp Olomouc, fakturační údaje, taneční sály a osoby pro členské příspěvky, ČSTS, Olymp Dance, Pro-Am a klubové vybavení.',
  path: '/kontakt',
});

const contacts = [
  {
    role: 'Předseda',
    name: 'Mgr. Miroslav Hýža',
    phone: '737 545 525',
    email: 'miroslav.hyza@tkolymp.cz',
    responsibilities: [
      'šéftrenér TK',
      'vnější vztahy',
      'koordinace interních agend',
      'rozvoj klubového webu',
    ],
  },
  {
    role: 'Ekonom',
    name: 'Ing. Roman Pecha',
    phone: '737 477 599',
    email: 'roman.pecha@tkolymp.cz',
    responsibilities: [
      'fakturace',
      'správa rozpočtu',
      'kontrola plateb',
      'vedení účetnictví',
    ],
  },
  {
    role: 'Tajemník',
    name: 'Ing. Filip Karásek',
    phone: '774 462 104',
    email: 'filip.karasek@tkolymp.cz',
    responsibilities: [
      'vedoucí trenér LAT',
      'členská agenda (komunikace, omluvenky, potvrzení, zápisy)',
      'komunikace s ČSTS (přestupy, hostování, přihlášky, příspěvky)',
    ],
  },
  {
    role: 'Marketing',
    name: 'Mgr. Pavel Grepl',
    phone: '724 227 101',
    email: 'pavel.grepl@tkolymp.cz',
    responsibilities: ['marketing a sociální sítě', 'kondiční příprava'],
  },
  {
    role: 'Klubové oblečení a doplňky',
    name: 'Mgr. Hana Anna Šišková',
    phone: '737 074 566',
    email: 'hanaanna.siskova@tkolymp.cz',
    responsibilities: ['zajištění klubového oblečení a doplňků'],
  },
  {
    role: 'Noví členové',
    name: 'Mgr. Žaneta Brizgalová',
    phone: '730 585 370',
    email: 'zaneta.brizgalova@gmail.com',
    responsibilities: ['nábor nových členů', 'fyzioterapie'],
  },
  {
    role: 'Taneční vystoupení',
    name: 'Mgr. Marie Hýžová ml.',
    phone: '737 644 899',
    email: 'marie.hyzova@tkolymp.cz',
    responsibilities: ['koordinace tanečních vystoupení'],
  },
  {
    role: 'Taneční kroužky Olymp Dance',
    name: 'Martin Matýsek',
    phone: '722 017 700',
    email: 'info@olympdance.cz',
    responsibilities: ['vedení projektu Olymp Dance (kroužky na školách a školkách)'],
  },
] satisfies readonly ContactCardProps[];

export default function ContactPage() {
  return (
    <>
      <PageHeader title="Kontakt" />
      <section className="my-4 rounded-md border border-accent-5 bg-accent-2 px-3 py-4">
        <h2 className="text-xl font-bold text-accent-11 mb-3">
          Fakturační údaje
        </h2>

        <div className="flex flex-col gap-5">
            <address className="mt-1 not-italic leading-snug text-neutral-12">
              <b>Taneční klub Olymp Olomouc, z. s.</b>
              <br />
              Jiráskova 381/25
              <br />
              Olomouc-Hodolany
              <br />
              779 00
            </address>

            <p className="mt-1 text-neutral-12">
              IČO: 68347286
              <br />
              Oddíl L, vložka 4133, Krajský soud v Ostravě, pobočka v Olomouci
              <br />
              Datová schránka: g2q66be
            </p>

            <p className="mt-1 text-neutral-12">
              Číslo účtu:{' '}
              <span className="font-semibold text-neutral-12">1806875329/0800</span>
            </p>
        </div>
      </section>

      <section
        className="col-feature my-6"
        aria-label="Kontaktní osoby"
      >
        <div className="grid gap-4 md:grid-cols-2 xl:grid-cols-3">
          {contacts.map((contact) => (
            <ContactCard key={contact.role} {...contact} />
          ))}
        </div>
      </section>
    </>
  );
}

type ContactCardProps = {
  role: string;
  name: string;
  phone?: string;
  email?: string;
  responsibilities?: readonly string[];
};

function ContactCard({
  role,
  name,
  phone,
  email,
  responsibilities = [],
}: ContactCardProps) {
  return (
    <article className={cardCls()}>
      <p className="text-sm font-medium uppercase leading-snug tracking-wide text-accent-11">
        {role}
      </p>
      <h2 className="mt-1.5 text-lg font-bold leading-tight text-neutral-12">
        {name}
      </h2>

      {(phone || email) && (
        <div className="mt-3 space-y-2 text-sm text-neutral-12">
          {phone && (
            <a
              className="flex w-fit max-w-full items-center gap-3 hover:text-accent-11 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-4 focus-visible:outline-accent-9"
              href={`tel:+420${phone.replaceAll(' ', '')}`}
            >
              <Phone aria-hidden="true" className="size-4 shrink-0 text-accent-10" />
              {phone}
            </a>
          )}
          {email && (
            <a
              className="flex max-w-full items-center gap-3 hover:text-accent-11 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-4 focus-visible:outline-accent-9"
              href={`mailto:${email}`}
            >
              <Mail aria-hidden="true" className="size-4 shrink-0 text-accent-10" />
              <span className="min-w-0 break-words">{email}</span>
            </a>
          )}
        </div>
      )}

      {responsibilities.length > 0 && (
        <ul className="mt-4 list-disc space-y-1 pl-5 text-sm leading-snug text-neutral-12 marker:text-accent-9">
          {responsibilities.map((responsibility) => (
            <li key={responsibility}>{responsibility}</li>
          ))}
        </ul>
      )}
    </article>
  );
}
