import { RichTextView } from '@app/ui/RichTextView';
import { CohortGroupDocument, CohortGroupFragment } from '@app/graphql/CohortGroup';
import { fetchGql } from '@app/graphql/query';
import { Card } from '@app/ui/Card';
import { Heading } from '@app/ui/Heading';
import { fromSlugArray, slugify } from '@app/ui/slugify';
import { GetStaticProps } from 'next';
import { NextSeo } from 'next-seo';
import type { NextPageWithLayout } from 'pages/_app';

type PageProps = {
  item: CohortGroupFragment;
};

const Page: NextPageWithLayout<PageProps> = ({ item }) => {
  return (
    <>
      <NextSeo title={item.name} />
      <Heading>{item.name}</Heading>
      <div className="container py-4">
        <RichTextView className="mb-10" value={item.description} />
        {item.skupiniesByCohortGroup.nodes.map((x) => (
          <Card key={x.id} cohort={x}>
            <h5 className="text-xl font-bold">{x.sName}</h5>
            <h6 className="font-bold mb-2">{x.sLocation}</h6>
            <RichTextView
              value={x.sDescription.replace('&nbsp;', ' ').replace('<br />', '')}
            />
          </Card>
        ))}
      </div>
    </>
  );
};

Page.showTopMenu = true;

export default Page;

export const getStaticPaths = () => ({ paths: [], fallback: 'blocking' });
export const getStaticProps: GetStaticProps<PageProps> = async (context) => {
  const id = fromSlugArray(context.params?.id) || fromSlugArray(context.params?.slug);
  const item = await fetchGql(CohortGroupDocument, {id}).then(x => x.cohortGroup);

  if (!item) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  if (fromSlugArray(context.params?.slug || '') !== slugify(`${item?.name}`)) {
    return {
      revalidate: 60,
      redirect: {
        destination: `/programy/${item.id}/${slugify(item.name)}`,
        permanent: false,
      },
    };
  }

  return {
    revalidate: 60,
    props: { item },
  };
};
