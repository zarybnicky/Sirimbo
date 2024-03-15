import { TitleBar } from '@/ui/TitleBar';
import { Layout } from '@/components/layout/Layout';
import React from 'react';
import { StringParam, useQueryParam } from 'use-query-params';
import { TabMenu } from '@/ui/TabMenu';
import { useQuery } from 'urql';
import { PersonAccountsDocument } from '@/graphql/Person';
import { moneyFormatter } from '@/ui/format';

const Page = () => {
  const [variant, setVariant] = useQueryParam('tab', StringParam);

  const tabs = [
    {
      id: 'info',
      label: <>Stav kreditu</>,
      contents: <PersonAccounts key="info" />,
    }
  ];

  return (
    <Layout requireAdmin>
      <TitleBar title="Platby" />

      <TabMenu selected={variant || tabs[0]?.id!} onSelect={setVariant} options={tabs} />
      <div className="mt-4">
        {(tabs.find(x => x.id === variant) || tabs[0])?.contents}
      </div>
    </Layout>
  );
};

const PersonAccounts = () => {
  const [{ data }] = useQuery({ query: PersonAccountsDocument });

  return <>
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

export default Page;
