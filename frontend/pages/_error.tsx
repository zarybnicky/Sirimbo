import { captureUnderscoreErrorException } from '@sentry/nextjs';
import type { NextPageContext } from 'next';
import NextErrorComponent, { type ErrorProps } from 'next/error';

export default function CustomErrorComponent(props: ErrorProps) {
  return <NextErrorComponent statusCode={props.statusCode} />;
}

CustomErrorComponent.getInitialProps = async (contextData: NextPageContext) => {
  await captureUnderscoreErrorException(contextData);
  return NextErrorComponent.getInitialProps(contextData);
};

CustomErrorComponent.showTopMenu = true;
