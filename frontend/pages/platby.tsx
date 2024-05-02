import { TitleBar } from '@/ui/TitleBar';
import { Layout } from '@/components/layout/Layout';
import React from 'react';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from '@/ui/TabMenu';
import { useQuery } from 'urql';
import { PersonAccountsDocument, UnpaidPaymentsDocument } from '@/graphql/Person';
import { describePosting, moneyFormatter } from '@/ui/format';
import { ExportBalanceSheetButton } from '@/ui/ExportBalanceSheetButton';

const Page = () => {
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
export default Page;

function AccountOverview() {
  const [{ data }] = useQuery({ query: PersonAccountsDocument });

  return <>
    <ExportBalanceSheetButton />
    {data?.filteredPeopleList?.map(x => (
      <div key={x.id} className="flex flex-wrap gap-2 justify-between even:bg-neutral-2 odd:bg-neutral-1 border-b">
        <span>{x.name}</span>
        <span>
          {moneyFormatter.format(parseFloat(x.accountsList.find(x => x)?.balance || 0))}
        </span>
      </div>
    ))}
  </>;
};

function UnpaidPayments() {
  const [{ data }] = useQuery({ query: UnpaidPaymentsDocument });
  const { unpaidPayments } = data || {};

  return (
    <>
      {unpaidPayments?.map((x) => (
        <div
          key={x.id}
          className="flex flex-wrap gap-2 justify-between even:bg-neutral-2 odd:bg-neutral-1 border-b"
        >
          <span>{describePosting(x.payment!)}</span>
          <span>{x.person?.name}</span>
          <span>{moneyFormatter.format(parseFloat(x.price?.amount))}</span>
        </div>
      ))}
    </>
  );
}
