/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require('react');

const CompLibrary = require('../../core/CompLibrary.js');

const MarkdownBlock = CompLibrary.MarkdownBlock; /* Used to read markdown */
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;

class HomeSplash extends React.Component {
    render() {
        const {siteConfig, language = ''} = this.props;
        const {baseUrl, docsUrl} = siteConfig;
        const docsPart = `${docsUrl ? `${docsUrl}/` : ''}`;
        const langPart = `${language ? `${language}/` : ''}`;
        const docUrl = doc => `${baseUrl}${docsPart}${langPart}${doc}`;

        const SplashContainer = props => (
            <div className="homeContainer">
                <div className="homeSplashFade">
                    <div className="wrapper homeWrapper">{props.children}</div>
                </div>
            </div>
        );

        const Logo = props => (
            <div className="projectLogo">
                <img src={props.img_src} alt="Project Logo"/>
            </div>
        );

        const ProjectTitle = () => (
            <h2 className="projectTitle">
                {siteConfig.title}
                <small>{siteConfig.tagline}</small>
            </h2>
        );

        const PromoSection = props => (
            <div className="section promoSection">
                <div className="promoRow">
                    <div className="pluginRowBlock">{props.children}</div>
                </div>
            </div>
        );

        const Button = props => (
            <div className="pluginWrapper buttonWrapper">
                <a className="button" href={props.href} target={props.target}>
                    {props.children}
                </a>
            </div>
        );

        return (
            <SplashContainer>
                <div className="inner">
                    <ProjectTitle siteConfig={siteConfig}/>
                    <PromoSection>
                        <Button href={docUrl('overview/overview_index')}>Overview</Button>
                        <Button href={docUrl('usecases/usecases_index')}>Use Cases</Button>
                        <Button href={docUrl('resources/resources_index')}>Resources</Button>
                        <Button href="https://github.com/zio/zio-sql" target="_blank">GitHub</Button>
                    </PromoSection>
                </div>
            </SplashContainer>
        );
    }
}

class Index extends React.Component {
    render() {
        const {config: siteConfig, language = ''} = this.props;
        const {baseUrl} = siteConfig;

        const Block = props => (
            <Container
                padding={['bottom', 'top']}
                id={props.id}
                background={props.background}>
                <GridBlock
                    align="center"
                    contents={props.children}
                    layout={props.layout}
                />
            </Container>
        );

        // const FeatureCallout = () => (
        //     <div
        //         className="productShowcaseSection paddingBottom"
        //         style={{textAlign: 'center'}}>
        //         <h2>Welcome to zio-sql</h2>
        //         <MarkdownBlock>
        //             TODO: Tagline
        //         </MarkdownBlock>

        //         <MarkdownBlock>
        //             TODO: Long description (paragraph)
        //         </MarkdownBlock>
        //     </div>
        // );

        // const Features = () => (
        //     <Block layout="fourColumn">
        //         {[
        //             {
        //                 content: 'TODO: Content 1',
        //                 image: `${baseUrl}img/undraw_tweetstorm.svg`,
        //                 imageAlign: 'top',
        //                 title: 'TODO: Title 1',
        //             },
        //             {
        //                 content: 'TODO: Content 2',
        //                 image: `${baseUrl}img/undraw_operating_system.svg`,
        //                 imageAlign: 'top',
        //                 title: 'TODO: Title 2',
        //             },
        //         ]}
        //     </Block>
        // );

        return (
            <div>
                <HomeSplash siteConfig={siteConfig} language={language}/>
                <div className="mainContainer">
                    {/* <Features/>
                    <FeatureCallout/> */}
                </div>
            </div>
        );
    }
}

module.exports = Index;
