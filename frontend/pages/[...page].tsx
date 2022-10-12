import * as React from 'react';
import { ReactPage } from '../components/ReactPage';
import { GetServerSideProps } from 'next';
import { usePageQuery } from 'index';
import { QueryClient } from '@tanstack/react-query';

const DynamicPage: React.FC<{ content: any; }> = ({ content }) => {
  return <ReactPage readOnly value={content} />;
}

export default DynamicPage;

export const getServerSideProps: GetServerSideProps = async (context) => {
  const { resolvedUrl: url } = context;

  const queryClient = new QueryClient();
  const data = await queryClient.fetchQuery(usePageQuery.getKey({ url }), usePageQuery.fetcher({ url }));
  if (!data.pageByUrl) {
    return { notFound: true };
  }

  return {
    props: {
      content: data.pageByUrl.content,
    },
  };
};
