import { RichTextView } from '@app/ui/RichTextView';
import { CohortGroupDocument, CohortGroupFragment } from '@app/graphql/CohortGroup';
import { fetchGql } from '@app/graphql/query';
import { Heading } from '@app/ui/Heading';
import { fromSlugArray, slugify } from '@app/ui/slugify';
import { GetStaticProps } from 'next';
import { NextSeo } from 'next-seo';
import { Layout } from 'components/layout/Layout';
import { CohortItem } from '@app/ui/CohortItem';

type PageProps = {
  item: CohortGroupFragment;
};

const Page: React.FC<PageProps> = ({ item }) => {
  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title={item.name} />
      <Heading>{item.name}</Heading>
      <div className="container py-4">
        <RichTextView className="mb-10" value={item.description} />
        {item.cohorts.nodes.map((x) => (
          <CohortItem key={x.id} id={x.id} />
        ))}
      </div>
    </Layout>
  );
};

export default Page;

export const getStaticPaths = () => ({ paths: [], fallback: 'blocking' });
export const getStaticProps: GetStaticProps<PageProps> = async (context) => {
  const id = fromSlugArray(context.params?.id) || fromSlugArray(context.params?.slug);
  const item = await fetchGql(CohortGroupDocument, { id }).then((x) => x.cohortGroup);

  if (!item) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  const slug = slugify(item.name);
  if (fromSlugArray(context.params?.slug || '') !== slug) {
    return {
      revalidate: 60,
      redirect: {
        destination: `/treninkove-programy/${item.id}/${slug}`,
        permanent: false,
      },
    };
  }

  return {
    revalidate: 60,
    props: { item },
  };
};
