'use client';

import { PaymentDocument } from '@/graphql/Payment';
import { slugify } from '@/lib/slugify';
import { DefinitionList } from '@/ui/DefinitionList';
import { EventButton } from '@/ui/EventButton';
import { moneyFormatter, numericFullFormatter } from '@/ui/format';
import { cardCls } from '@/ui/style';
import { PageHeader } from '@/ui/PageHeader';
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
        breadcrumbLabel="Platba"
        breadcrumbs={[{ label: 'Platby', href: '/platby' }]}
      />

      {fetching && <p>Načítám…</p>}
      {error && <p className="text-accent-11">Nepodařilo se načíst platbu.</p>}
      {!fetching && !error && !payment && <p>Platba nebyla nalezena.</p>}
      {payment && (
        <div className="space-y-4">
          <DefinitionList
            items={{
              ID: payment.id,
              Stav: payment.status,
              Vytvořena: numericFullFormatter.format(new Date(payment.createdAt)),
              Splatnost:
                payment.dueAt && numericFullFormatter.format(new Date(payment.dueAt)),
              Uhrazena:
                payment.paidAt && numericFullFormatter.format(new Date(payment.paidAt)),
              'Variabilní symbol': payment.variableSymbol,
              'Specifický symbol': payment.specificSymbol,
            }}
          />

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

          {payment.event && (
            <div>
              <h3 className="text-lg font-semibold">Za událost</h3>
              <EventButton instance={payment.event} viewer="auto" />
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
