import * as React from 'react';
import { ReactPage } from '../components/ReactPage';
import { GetServerSideProps } from 'next';
import { usePageQuery } from 'lib/graphql/Page';

export default function DynamicPage({ content }: { content: any }) {
  return <ReactPage readOnly value={content} />;
}

export const getServerSideProps: GetServerSideProps = async (context) => {
  const { resolvedUrl: url } = context;

  const data = await usePageQuery.fetcher({ url })();
  if (!data.pageByUrl) {
    return { notFound: true };
  }

  return {
    props: {
      content: data.pageByUrl.content,
    },
  };
};
