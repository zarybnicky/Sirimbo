/* eslint-disable import-x/no-unused-modules */
import Link from 'next/link';

export default function NotFound() {
  return (
    <section className="py-16">
      <h1 className="text-4xl font-bold text-accent-12">Stránka nenalezena</h1>
      <p className="mt-4 text-neutral-11">
        Požadovaná stránka neexistuje nebo už není dostupná.
      </p>
      <Link
        href="/"
        className="mt-8 inline-flex rounded-xl bg-accent-9 px-4 py-2 text-sm font-medium text-accent-0 hover:bg-accent-10"
      >
        Zpět na úvod
      </Link>
    </section>
  );
}
