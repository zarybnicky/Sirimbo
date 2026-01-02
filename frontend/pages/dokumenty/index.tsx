import { Combobox } from '@/ui/fields/Combobox';
import { FileListDocument } from '@/graphql/Documents';
import { fullDateFormatter } from '@/ui/format';
import { useQuery } from 'urql';
import { TitleBar } from '@/ui/TitleBar';
import { origin } from '@/graphql/query';
import { NextSeo } from 'next-seo';
import { Layout } from '@/ui/Layout';
import { cardCls } from '@/ui/style';
import React from 'react';

const categories = [
  { id: '1', label: 'Schůze,\u{00A0}rady' },
  { id: '2', label: 'Soutěže' },
  { id: '3', label: 'Soustředění' },
  { id: '0', label: 'Ostatní' },
];

export default function DocumentsPage() {
  const [category, setCategory] = React.useState<string | null | undefined>();

  const [{ data }] = useQuery({
    query: FileListDocument,
    variables: {
      category: category ? Number.parseInt(category, 10) : undefined,
    },
  });

  return (
    <Layout requireMember>
      <div className="col-feature py-4 lg:pb-8">
        <NextSeo title="Dokumenty" />
        <TitleBar title="Dokumenty">
          <Combobox
            value={category}
            align="end"
            placeholder="všechny dokumenty"
            options={categories}
            onChange={setCategory}
          />
        </TitleBar>

        {data?.dokumentiesList?.map((row, i) => (
          <div key={i} className={cardCls()}>
            <a
              target="_blank"
              rel="noreferrer"
              href={`${origin}/member/download?id=${row.id}`}
              className="flex justify-between"
            >
              <span>{row.dName}</span>
              <div className="flex gap-4">
                <span>
                  {row.dTimestamp
                    ? fullDateFormatter.format(new Date(row.dTimestamp))
                    : ''}
                </span>
                <span>
                  {categories.find((x) => x.id === row.dKategorie.toString())?.label}
                </span>
              </div>
            </a>
          </div>
        ))}
      </div>
    </Layout>
  );
}
