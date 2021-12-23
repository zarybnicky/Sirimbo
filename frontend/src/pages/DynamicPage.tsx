import * as React from 'react';
import { gql, useQuery } from '@apollo/client';
import { useLocation } from 'react-router-dom';
import { ReactPage } from '../components/ReactPage';

const GET_PAGE = gql(`
query GetPage($url: String!) {
  pageByUrl(url: $url) {
    content
  }
}`);

export const DynamicPage = () => {
  const location = useLocation();
  const { data } = useQuery(GET_PAGE, { variables: { url: location.pathname } });
  return <ReactPage readOnly value={data?.pageByUrl?.content} />;
}
