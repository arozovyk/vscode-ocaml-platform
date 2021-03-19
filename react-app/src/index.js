import React from "react";
import ReactDOM from "react-dom";
import '../css/style.css';
import InteractorFactory from './Interaction/InteractorFactory';
import "regenerator-runtime/runtime";
import ASTOutput from './components/ASTOutput';
import astExample from './containers/test.json'
import getTreeAdapter from './parserMiddleware'
import newParser from './parsers/refmt-ml'
const Interactor = InteractorFactory.create();

class Index extends React.Component {

  constructor(props) {
    super(props);
    this.state = { ast: '' }
    this.state = { directoryInfo: "" };
    window.addEventListener('message', event => {
      this.setState({
        ast: event.data.value
      });
    });

  }




  updateFilesToDisplay() {
    Interactor.getDirectoryInfo(directoryInfo => {
      this.setState({ directoryInfo: directoryInfo });
    })
  }

  render() {

    const treeAdapter = getTreeAdapter(newParser)

    const astExampleResult = {
      ast: astExample,
      error: null,
      time: 120,
      treeAdapter: treeAdapter
    }

    return <>
      <div className="container">test{this.state.ast}
        <ASTOutput parseResult={astExampleResult} position={0} />
        {this.message}
      </div>
    </>
  }
}

ReactDOM.render(<Index />, document.getElementById("index"));