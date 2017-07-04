#ifndef COLLAPSED_CONSUMER_HPP
#define COLLAPSED_CONSUMER_HPP

#include "Util.hpp"
#include "Features.hpp"

namespace sam {


template  <typename TupleType,
          size_t... keyFields>
class CollapsedConsumer : public BaseComputation, 
                          public AbstractConsumer<TupleType>
{
private:
  /**
   * The collapsed consumer applies an aggregation function to a list
   * of features.  This function is specified at construction time.
   */
  std::function<double(std::list<std::shared_ptr<Feature>>)> func;

  /**
   * The collapsed consumer is aggregating a list of features with
   * a targetId.  This is specified at constuction time. 
   */
  std::string targetId;

public:
  /**
   * \param _func The function that will be applied to a list of features.
   *
   */
  CollapsedConsumer(
            std::function<double(std::list<std::shared_ptr<Feature>>)> _func,
            std::string _targetId,
            size_t nodeId,
            FeatureMap& featureMap,
            std::string newIdentifier) :
            func(_func),
            targetId(_targetId),
            BaseComputation(nodeId, featureMap, newIdentifier)
  {
    
  }
 
  bool consume(TupleType const& tuple)
  {
    std::string key = generateKey<keyFields...>(tuple);

    if (featureMap.exists(key, targetId))
    {
      auto mapFeature = std::static_pointer_cast<const MapFeature>(
                          this->featureMap.at(key, targetId));
      double result = mapFeature->evaluate(func);
       
      SingleFeature feature(result);
      this->featureMap.updateInsert(key, this->identifier, feature);
  
      return true;   
    }

    return false;

  }
  
};


}

#endif
