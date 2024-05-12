import { useForm } from 'react-hook-form';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { FileListDocument } from '@/graphql/Documents';
import { fullDateFormatter } from '@/ui/format';
import { Card } from '@/ui/Card';
import { useQuery } from 'urql';
import { TitleBar } from '@/ui/TitleBar';
import { origin } from '@/graphql/query';
import { NextSeo } from 'next-seo';
import { Layout } from '@/components/layout/Layout';

const categories = [
  { id: '1', label: 'Schůze,\u{00A0}rady' },
  { id: '2', label: 'Soutěže' },
  { id: '3', label: 'Soustředění' },
  { id: '0', label: 'Ostatní' },
];

const Page = () => {
  const { control, watch } = useForm<{ category: string }>();
  const category = watch('category');

  const [{ data }] = useQuery({
    query: FileListDocument,
    variables: {
      category: category ? parseInt(category, 10) : undefined,
    },
  });

  return (
    <Layout requireMember>
    <div className="col-feature py-4 lg:pb-8">
      <NextSeo title="Dokumenty" />
      <TitleBar title="Dokumenty">
        <ComboboxElement
          align="end"
          control={control}
          name="category"
          placeholder="všechny dokumenty"
          options={categories}
        />
      </TitleBar>

      {data?.dokumentiesList?.map((row, i) => (
        <Card key={i}>
          <a
            target="_blank"
            rel="noreferrer"
            href={`${origin}/member/download?id=${row.id}`}
            className="flex justify-between"
          >
            <span>{row.dName}</span>
            <div className="flex gap-4">
              <span>
                {row.dTimestamp ? fullDateFormatter.format(new Date(row.dTimestamp)) : ''}
              </span>
              <span>
                {categories.find((x) => x.id === row.dKategorie.toString())?.label}
              </span>
            </div>
          </a>
        </Card>
      ))}
    </div>
    </Layout>
  );
};


export default Page;
