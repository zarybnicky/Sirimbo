'use client';

import { FileQuestion, RefreshCw, TriangleAlert } from 'lucide-react';
import Link from 'next/link';
import { buttonCls, typographyCls } from '@/ui/style';

type StatusPageProps = {
  reset?: () => void;
  status: 'error' | 'not-found';
};

const content = {
  error: {
    title: 'Tato stránka se nepodařila načíst',
    details:
      'Zkuste to prosím znovu. Pokud potíže přetrvávají, vraťte se na úvodní stránku.',
    Icon: TriangleAlert,
  },
  'not-found': {
    title: 'Stránka nenalezena',
    details: 'Požadovaná stránka neexistuje nebo už není dostupná.',
    Icon: FileQuestion,
  },
};

export function StatusPage({ reset, status }: StatusPageProps) {
  const { details, Icon, title } = content[status];

  return (
    <section className="col-feature grid min-h-[60vh] place-items-center py-16">
      <div className="max-w-lg text-center">
        <div className="inline-flex rounded-full bg-accent-4 p-4 text-accent-10">
          <Icon aria-hidden="true" className="size-12" strokeWidth={1.75} />
        </div>
        <h1 className={typographyCls({ className: 'mt-6' })}>{title}</h1>
        <p
          className="mt-4 text-neutral-11"
          role={status === 'error' ? 'alert' : undefined}
        >
          {details}
        </p>
        <div className="mt-8 flex flex-wrap justify-center gap-3">
          {reset && status === 'error' && (
            <button
              type="button"
              onClick={reset}
              className={buttonCls({ className: 'gap-2' })}
            >
              <RefreshCw aria-hidden="true" className="size-4" />
              Zkusit znovu
            </button>
          )}
          <Link
            href="/"
            className={buttonCls({ variant: reset ? 'outline' : 'primary' })}
          >
            Zpět na úvod
          </Link>
        </div>
      </div>
    </section>
  );
}
