import React from "react";
import { useRouter } from "next/router";
import { useAuth } from "./data/use-auth";
import { Container, Typography } from "@mui/material";

export const withUserLoggedOut = <P extends JSX.IntrinsicAttributes>(WrappedComponent: React.FunctionComponent<P>) => {
  return (props: P) => {
    const router = useRouter();
    const { user, isLoading } = useAuth();

    React.useEffect(() => {
      if (user && !isLoading) {
        router.replace("/dashboard");
      }
    }, [user, isLoading]);

    return <WrappedComponent {...props} />;
  };
};

export const withUserLoggedIn = <P extends JSX.IntrinsicAttributes>(WrappedComponent: React.FunctionComponent<P>) => {
  return (props: P) => {
    const router = useRouter();
    const { user, isLoading } = useAuth();

    React.useEffect(() => {
      if (!user && !isLoading) {
        router.replace("/login");
      }
    }, [user, isLoading]);

    if (!user || isLoading) {
      return <Container maxWidth="lg" sx={{ margin: '4rem auto 6rem' }}>
        <Typography variant="h5">Načítám...</Typography>
      </Container>;
    }

    return <WrappedComponent {...props} />;
  };
};
