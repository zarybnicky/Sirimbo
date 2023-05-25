import { Card } from 'components/Card';
import { Heading } from 'components/Heading';
import { Item } from 'components/layout/Item';
import { RichTextView } from 'components/RichTextView';
import { CohortGroupFragment, CohortGroupDocument } from 'lib/graphql/CohortGroup';
import { fetchGql } from 'lib/query';
import { fromSlugArray, slugify } from 'lib/slugify';
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
      <Item>
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
      </Item>
    </>
  );
};

Page.showTopMenu = true;

export default Page;

export const getStaticPaths = async () => ({ paths: [], fallback: 'blocking' });
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
