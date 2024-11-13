import { TitleBar } from '@/ui/TitleBar';
import { Layout } from '@/components/layout/Layout';
import React from 'react';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from '@/ui/TabMenu';
import { useMutation, useQuery } from 'urql';
import { PersonAccountsDocument, UnpaidPaymentsDocument } from '@/graphql/Person';
import { describePosting, moneyFormatter } from '@/ui/format';
import { DropdownMenu, DropdownMenuButton, DropdownMenuContent, DropdownMenuTrigger } from '@/ui/dropdown';
import { MarkAsPaidDocument } from '@/graphql/Payment';
import { buttonCls } from '@/ui/style';
import { exportBalanceSheet } from '@/ui/reports/export-balance-sheet';

export default function PaymentsPage() {
  const [tab, setTab] = useQueryParam('tab', StringParam);

  const tabs = [
    {
      id: 'info',
      label: <>Stav kreditu</>,
      contents: () => <AccountOverview key="info" />,
    },
    {
      id: 'unpaid',
      label: <>Nezaplacené</>,
      contents: () => <UnpaidPayments key="unpaid" />,
    },
  ];

  return (
    <Layout requireAdmin>
      <TitleBar title="Platby" />

      <TabMenu selected={tab || tabs[0]?.id!} onSelect={setTab} options={tabs} />
      <div className="mt-4">
        {(tabs.find(x => x.id === tab) || tabs[0])?.contents()}
      </div>
    </Layout>
  );
};

function AccountOverview() {
  const [{ data }] = useQuery({ query: PersonAccountsDocument });

  return <>
    <button type="button" className={buttonCls()} onClick={exportBalanceSheet}>
      Přehled plateb 2023
    </button>
    {data?.filteredPeopleList?.map(x => (
      <div key={x.id} className="flex flex-wrap gap-2 justify-between even:bg-neutral-2 odd:bg-neutral-1 border-b">
        <span>{x.name}</span>
        <span>
          {moneyFormatter.format(Number.parseFloat(x.accountsList.find(x => x)?.balance || 0))}
        </span>
      </div>
    ))}
  </>;
};

function UnpaidPayments() {
  const [{ data }] = useQuery({ query: UnpaidPaymentsDocument });
  const { unpaidPayments } = data || {};
  const markAsPaid = useMutation(MarkAsPaidDocument)[1];

  return (
    <>
      {unpaidPayments?.map((x) => (
        <div
          key={x.id}
          className="flex flex-wrap gap-4 justify-between even:bg-neutral-2 odd:bg-neutral-1 border-b"
        >
          <span className="grow">{x.person?.name}</span>
          <span>{describePosting(x.payment!)}</span>
          <span>{moneyFormatter.format(Number.parseFloat(x.price?.amount))}</span>
          <span>
            <DropdownMenu>
              <DropdownMenuTrigger.CornerDots className="relative top-0 right-0" />
              <DropdownMenuContent align="end">
                <DropdownMenuButton onClick={() => markAsPaid({ id: x.payment?.id! })}>
                  Označit jako zaplacenou
                </DropdownMenuButton>
              </DropdownMenuContent>
            </DropdownMenu>
          </span>
        </div>
      ))}
    </>
  );
}
