'use client';

import { PaymentDocument } from '@/graphql/Payment';
import { slugify } from '@/lib/slugify';
import { EventButton } from '@/ui/EventButton';
import { moneyFormatter, numericFullFormatter } from '@/ui/format';
import { cardCls } from '@/ui/style';
import { PageHeader } from '@/ui/TitleBar';
import Link from 'next/link';
import { useQuery } from 'urql';

export function Payment({ id }: { id: string }) {
  const [{ data, fetching, error }] = useQuery({
    query: PaymentDocument,
    variables: { id },
    pause: !id,
  });

  const payment = data?.payment;
  const pageTitle = payment ? `Detail platby ${id}` : 'Načítám...';

  return (
    <>
      <PageHeader
        title={pageTitle}
        breadcrumbs={[{ label: 'Platby', href: '/platby' }, { label: pageTitle }]}
      />

      {fetching && <p>Načítám…</p>}
      {error && <p className="text-accent-11">Nepodařilo se načíst platbu.</p>}
      {!fetching && !error && !payment && <p>Platba nebyla nalezena.</p>}
      {payment && (
        <div className="space-y-4">
          <dl className="text-sm tabular">
            <dt>ID</dt>
            <dd>{payment.id}</dd>
            <dt>Stav</dt>
            <dd>{payment.status}</dd>
            <dt>Vytvořena</dt>
            <dd>{numericFullFormatter.format(new Date(payment.createdAt))}</dd>
            <dt>Splatnost</dt>
            {payment.dueAt && (
              <dd>{numericFullFormatter.format(new Date(payment.dueAt))}</dd>
            )}
            <dt>Uhrazena</dt>
            {payment.paidAt && (
              <dd>{numericFullFormatter.format(new Date(payment.paidAt))}</dd>
            )}
            <dt>Variabilní symbol</dt>
            <dd>{payment.variableSymbol}</dd>
            <dt>Specifický symbol</dt>
            <dd>{payment.specificSymbol}</dd>
          </dl>

          {payment.cohortSubscription && (
            <div>
              <h3 className="text-lg font-semibold">Členské příspěvky</h3>
              <p className="flex flex-wrap items-center gap-2">
                {payment.cohortSubscription.cohort && (
                  <Link
                    href={`/treninkove-skupiny/${payment.cohortSubscription.cohort.id}/${slugify(payment.cohortSubscription.cohort.name)}`}
                    className="text-sm font-medium text-accent-11 hover:underline"
                  >
                    {payment.cohortSubscription.cohort.name}
                  </Link>
                )}
              </p>
            </div>
          )}

          {payment.eventInstance && (
            <div>
              <h3 className="text-lg font-semibold">Za událost</h3>
              <EventButton instance={payment.eventInstance} viewer="auto" />
            </div>
          )}

          {payment.transactions.nodes.length > 0 && (
            <section>
              <h2 className="text-lg font-semibold">Transakce</h2>
              {payment.transactions.nodes.map((tran) => (
                <article key={tran.id} className={cardCls({ className: 'space-y-2' })}>
                  <h3 className="font-medium">
                    {numericFullFormatter.format(new Date(tran.effectiveDate))}
                  </h3>
                  <p className="text-sm">{tran.description}</p>

                  {tran.postingsList.map(({ id, account, amount }) => (
                    <div key={id} className="flex flex-wrap justify-between text-sm">
                      <span>
                        {!account ? (
                          '?'
                        ) : account.personId ? (
                          <Link
                            className="hover:underline"
                            href={`/clenove/${account.personId}`}
                          >
                            {account.person?.name ?? '?'}
                          </Link>
                        ) : (
                          `Na účet klubu`
                        )}
                      </span>
                      <span>{moneyFormatter.format({ amount, currency: 'CZK' })}</span>
                    </div>
                  ))}
                </article>
              ))}
            </section>
          )}
        </div>
      )}
    </>
  );
}
