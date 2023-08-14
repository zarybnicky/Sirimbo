import { CoupleView } from '@app/ui/CoupleView';
import { fromSlugArray } from '@app/ui/slugify';
import { useRouter } from 'next/router';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';

const Page = () => (
  <Layout>
    <NextSeo title="Páry" />
    <CoupleView id={fromSlugArray(useRouter().query.id)} />
  </Layout>
);

export default Page;
